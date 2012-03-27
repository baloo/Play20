package play.api.libs.json

import org.codehaus.jackson.{ JsonGenerator, JsonToken, JsonParser }
import org.codehaus.jackson.`type`.JavaType
import org.codehaus.jackson.map._
import org.codehaus.jackson.map.annotate.JsonCachable
import org.codehaus.jackson.map.`type`.{ TypeFactory, ArrayType }

import scala.collection._

import scala.collection.immutable.Stack
import scala.annotation.tailrec

object Implicits{
  implicit val string = {(s: String) => JsString(s)}
}

/**
 * Lenses
 */

trait Lens[A,B]{
  self =>

  def get: A => B
  def set: (A,B) => A

  def mod(a:A, f: B => B) : A = set(a, f(get(a)))

  def compose[C,L <: Lens[C,B]](that: Lens[C,A])(implicit cons:LensConstructor[C,B,L]):L = cons(
    c => get(that.get(c)),
    (c, b) => that.mod(c, set(_, b))
  )

  def andThen[C,L <: Lens[A,C]](o: Lens[B,C])(implicit cons:LensConstructor[A,C,L]) = o.compose(self)(cons)

}

trait LensConstructor[A,B,L <: Lens[A,B]] extends (( A => B, (A,B) => A) => L)

trait PriorityOne {
  implicit def lens[A,B,L <: Lens[A,B]] = new LensConstructor[A,B,Lens[A,B]] {

    def apply(get: A => B,
              set: (A, B) => A) = Lens(get,set)

  }
}

object LensConstructor extends PriorityOne {

  implicit val jsValueLens = new LensConstructor[JsValue,JsValue,JsValueLens] {

    def apply(get: JsValue => JsValue,
              set: (JsValue, JsValue) => JsValue) = JsValueLens(get,set)

  }
}

object Lens {
  def apply[A,B](getter: A => B,setter: (A, B) => A) = new Lens[A,B] {
    def get = getter
    def set = setter
  }
}

case class JsValueLens(getter: JsValue => JsValue,
                setter: (JsValue, JsValue) => JsValue) extends Lens[JsValue,JsValue]{

  def get = getter
  def set = setter               

  def apply(whole: JsValue) = get(whole)

  def apply(whole: JsValue, repl: JsValue) = set(whole, repl)

  def \(f: String) = JsValueLens(
    JsValueLens.objectGetter(get)(f),
    JsValueLens.objectSetter(get)(set)(f))

  def at(i: Int) = JsValueLens(
    JsValueLens.arrayGetter(get)(i),
    JsValueLens.arraySetter(get)(set)(i))

  def as[A](implicit format:Format[A]):Lens[JsValue,A] = Lens[JsValue,A](
    getter = jsValue => format.reads(get(jsValue)), 
    setter = (me, value) => set(me, format.writes(value)) )

  def asEither[A](implicit format:Format[A]): Lens[JsValue,Either[String, A]] = Lens[JsValue,Either[String, A]](
    getter = jsValue => get(jsValue) match {
      case JsUndefined(e) => Left(e)
      case e => Right(format.reads(e))
    },
    setter = (me, value) => value match {
      case Left(_) => me
      case Right(v) => set(me, format.writes(v))
    })

  def asOpt[A](implicit format:Format[A]): Lens[JsValue,Option[A]] =
    this.asEither andThen Lens[Either[String,A],Option[A]](
      getter = jsValue => jsValue match {
        case Left(_) => None
        case Right(v) => Some(v)
      },
      setter = (me, value) => value match {
        case None => me
        case Some(v) => me match {
          case l if l.isLeft => l
          case Right(_) => Right[String,A](v)
        }
      })

  

}

object JsValueLens {
  def init = JsValueLens(a => a, (_, a) => a)
  def self = JsValueLens(a => a, (a, _) => a)

  def \(f: String): JsValueLens = JsValueLens(
    objectGetter(a => a)(f),
    objectSetter(a => a)((_, a) => a)(f)
  )

  def at(i: Int): JsValueLens = JsValueLens(
    arrayGetter(a => a)(i),
    arraySetter(a => a)((_, a) => a)(i)
  )

  private[JsValueLens] def objectGetter
    (get: JsValue => JsValue)
    (f: String): (JsValue => JsValue) = (a => get(a) match {
        case JsObject(fields) => 
          fields
            .find(t => t._1 == f)
            .getOrElse(("undefined" -> JsUndefined("Field " + f + " not found")))
            ._2
        case _ => JsUndefined("Element is not an object, couldn't find field "+f)
      })

  private[JsValueLens] def arrayGetter
    (get: JsValue => JsValue)
    (i: Int): (JsValue => JsValue) = (a => {
      get(a) match {
        case JsArray(fields) => fields
          .lift(i)
          .getOrElse(JsUndefined("Index " + i + " not found"))
        case _ => JsUndefined("Element is not an array, couldn't find index " + i)
      }
    })

  private[JsValueLens] def objectSetter
    (get: JsValue => JsValue)
    (set: (JsValue, JsValue) => JsValue)
    (f: String): ((JsValue, JsValue) => JsValue) = (whole, repl) => {
      get(whole) match {
        case JsObject(fields) => {
          val found = fields.find(t => t._1 == f) match {
            case None => false
            case _ => true
          }
          set(whole, JsObject(found match {
            case false => fields :+ (f -> repl)
            case true => fields.map{
              t => t match {
                case (k: String, v: JsValue) => if(k == f){
                  k -> repl
                }else{
                  k -> v
                }
              }
            }
          }))
        }
        case _ => {
          set(whole, JsObject(Seq(f -> repl)))
        }
      }
    }

  private[JsValueLens] def arraySetter
    (get: JsValue => JsValue)
    (set: (JsValue, JsValue) => JsValue)
    (i: Int): ((JsValue, JsValue) => JsValue) = (whole, repl) => {
      get(whole) match {
        case JsArray(fields) => {
          val found = fields.lift(i) match {
            case None => false
            case _ => true
          }
          set(whole, JsArray(found match {
            case false => fields :+ repl
            case true => fields.patch(i, Seq(repl), 1)
          }))
        }
        case _ => {
          set(whole, JsArray(Seq(repl)))
        }
      }
    }


}

/**
 * Generic json value
 */
sealed trait JsValue {
  import scala.util.control.Exception._

  /**
   * Return the property corresponding to the fieldName, supposing we have a JsObject.
   *
   * @param fieldName the name of the property to lookup
   * @return the resulting JsValue. If the current node is not a JsObject or doesn't have the property, a JsUndefined will be returned.
   */
  def \(fieldName: String): JsValue = JsUndefined("'" + fieldName + "'" + " is undefined on object: " + this)

  /**
   * Return the element at a given index, supposing we have a JsArray.
   *
   * @param idx the index to lookup
   * @param the resulting JsValue. If the current node is not a JsArray or the index is out of bounds, a JsUndefined will be returned.
   */
  def apply(idx: Int): JsValue = JsUndefined(this.toString + " is not an array")

  /**
   * Lookup for fieldName in the current object and all descendants.
   *
   * @return the list of matching nodes
   */
  def \\(fieldName: String): Seq[JsValue] = Nil

  /**
   * Tries to convert the node into a T. An implicit Reads[T] must be defined.
   *
   * @return Some[T] if it succeeds, None if it fails.
   */
  def asOpt[T](implicit fjs: Reads[T]): Option[T] = catching(classOf[RuntimeException]).opt(fjs.reads(this))

  /**
   * Tries to convert the node into a T, throwing an exception if it can't. An implicit Reads[T] must be defined.
   */
  def as[T](implicit fjs: Reads[T]): T = fjs.reads(this)

  override def toString = Json.stringify(this)

}

/**
 * Represent a Json null value.
 */
case object JsNull extends JsValue

/**
 * Represent a missing Json value.
 */
case class JsUndefined(error: String) extends JsValue

/**
 * Represent a Json boolean value.
 */
case class JsBoolean(value: Boolean) extends JsValue

/**
 * Represent a Json number value.
 */
case class JsNumber(value: BigDecimal) extends JsValue

/**
 * Represent a Json string value.
 */
case class JsString(value: String) extends JsValue

/**
 * Represent a Json arayy value.
 */
case class JsArray(value: Seq[JsValue] = List()) extends JsValue {

  /**
   * Access a value of this array.
   *
   * @param index Element index.
   */
  override def apply(index: Int): JsValue = {
    value.lift(index).getOrElse(JsUndefined("Array index out of bounds in " + this))
  }

  /**
   * Lookup for fieldName in the current object and all descendants.
   *
   * @return the list of matching nodes
   */
  override def \\(fieldName: String): Seq[JsValue] = value.flatMap(_ \\ fieldName)

  /**
   * Concatenates this array with the elements of an other array.
   */
  def ++(other: JsArray): JsArray = JsArray(value ++ other.value)

  /**
   * Append an element to this array.
   */
  def :+(el: JsValue): JsArray = JsArray(value :+ el)

  /**
   * Prepend an element to this array.
   */
  def +:(el: JsValue): JsArray = JsArray(el +: value)

}

/**
 * Represent a Json object value.
 */
case class JsObject(fields: Seq[(String, JsValue)]) extends JsValue {

  lazy val value: Map[String, JsValue] = fields.toMap

  /**
   * Return the property corresponding to the fieldName, supposing we have a JsObject.
   *
   * @param fieldName the name of the property to lookup
   * @return the resulting JsValue. If the current node is not a JsObject or doesn't have the property, a JsUndefined will be returned.
   */
  override def \(fieldName: String): JsValue = value.get(fieldName).getOrElse(super.\(fieldName))

  /**
   * Lookup for fieldName in the current object and all descendants.
   *
   * @return the list of matching nodes
   */
  override def \\(fieldName: String): Seq[JsValue] = {
    value.foldLeft(Seq[JsValue]())((o, pair) => pair match {
      case (key, value) if key == fieldName => o ++ (value +: (value \\ fieldName))
      case (_, value) => o ++ (value \\ fieldName)
    })
  }

  /**
   * Return all keys
   */
  def keys: Set[String] = fields.map(_._1).toSet

  /**
   * Return all values
   */
  def values: Set[JsValue] = fields.map(_._2).toSet

  /**
   * Merge this object with an other one. Values from other override value of the current object.
   */
  def ++(other: JsObject) = JsObject(fields.filterNot(field => other.keys(field._1)) ++ other.fields)

}

// -- Serializers.

@JsonCachable
private[json] class JsValueSerializer extends JsonSerializer[JsValue] {

  def serialize(value: JsValue, json: JsonGenerator, provider: SerializerProvider) {
    value match {
      case JsNumber(v) => json.writeNumber(v.bigDecimal)
      case JsString(v) => json.writeString(v)
      case JsBoolean(v) => json.writeBoolean(v)
      case JsArray(elements) => json.writeObject(elements)
      case JsObject(values) => {
        json.writeStartObject()
        values.filterNot( _.isInstanceOf[JsUndefined]).foreach { t =>
          json.writeFieldName(t._1)
          json.writeObject(t._2)
        }
        json.writeEndObject()
      }
      case JsNull => json.writeNull()
      case JsUndefined(error) => {
        play.Logger.warn("Serializing an object with an undefined property: " + error)
      }
    }
  }
}

private[json] sealed trait DeserializerContext {
  def addValue(value: JsValue): DeserializerContext
}

private[json] case class ReadingList(content: List[JsValue]) extends DeserializerContext {
  override def addValue(value: JsValue): DeserializerContext = {
    ReadingList(content :+ value)
  }
}

// Context for reading an Object
private[json] case class KeyRead(content: List[(String, JsValue)], fieldName: String) extends DeserializerContext {
  def addValue(value: JsValue): DeserializerContext = ReadingMap(content :+ (fieldName -> value))
}

// Context for reading one item of an Object (we already red fieldName)
private[json] case class ReadingMap(content: List[(String, JsValue)]) extends DeserializerContext {

  def setField(fieldName: String) = KeyRead(content, fieldName)
  def addValue(value: JsValue): DeserializerContext = throw new Exception("Cannot add a value on an object without a key, malformed JSON object!")

}

@JsonCachable
private[json] class JsValueDeserializer(factory: TypeFactory, klass: Class[_]) extends JsonDeserializer[Object] {
  def deserialize(jp: JsonParser, ctxt: DeserializationContext): JsValue = {
    val value = deserialize(jp, ctxt, List())

    if (!klass.isAssignableFrom(value.getClass)) {
      throw ctxt.mappingException(klass)
    }
    value
  }

  @tailrec
  final def deserialize(jp: JsonParser, ctxt: DeserializationContext, parserContext: List[DeserializerContext]): JsValue = {
    if (jp.getCurrentToken == null) {
      jp.nextToken()
    }

    val (maybeValue, nextContext) = (jp.getCurrentToken, parserContext) match {

      case (JsonToken.VALUE_NUMBER_INT, c) => (Some(JsNumber(jp.getLongValue)), c)

      case (JsonToken.VALUE_NUMBER_FLOAT, c) => (Some(JsNumber(jp.getDoubleValue)), c)

      case (JsonToken.VALUE_STRING, c) => (Some(JsString(jp.getText)), c)

      case (JsonToken.VALUE_TRUE, c) => (Some(JsBoolean(true)), c)

      case (JsonToken.VALUE_FALSE, c) => (Some(JsBoolean(false)), c)

      case (JsonToken.VALUE_NULL, c) => (Some(JsNull), c)

      case (JsonToken.START_ARRAY, c) => (None, (ReadingList(List())) +: c)

      case (JsonToken.END_ARRAY, ReadingList(content) :: stack) => (Some(JsArray(content)), stack)

      case (JsonToken.END_ARRAY, _) => throw new RuntimeException("We should have been reading list, something got wrong")

      case (JsonToken.START_OBJECT, c) => (None, ReadingMap(List()) +: c)

      case (JsonToken.FIELD_NAME, (c: ReadingMap) :: stack) => (None, c.setField(jp.getCurrentName) +: stack)

      case (JsonToken.FIELD_NAME, _) => throw new RuntimeException("We should be reading map, something got wrong")

      case (JsonToken.END_OBJECT, ReadingMap(content) :: stack) => (Some(JsObject(content)), stack)

      case (JsonToken.END_OBJECT, _) => throw new RuntimeException("We should have been reading an object, something got wrong")

      case _ => throw ctxt.mappingException(classOf[JsValue])
    }

    // Read ahead
    jp.nextToken()

    maybeValue match {
      case Some(v) if nextContext.isEmpty && jp.getCurrentToken == null =>
        //done, no more tokens and got a value!
        v

      case Some(v) if nextContext.isEmpty =>
        //strange, got value, but there is more tokens and have no prior context!
        throw new Exception("Malformed JSON: Got a sequence of JsValue outside an array or an object.")

      case maybeValue =>
        val toPass = maybeValue.map { v =>
          val previous :: stack = nextContext
          (previous.addValue(v)) +: stack
        }.getOrElse(nextContext)

        deserialize(jp, ctxt, toPass)

    }

  }
}

private[json] class PlayDeserializers(classLoader: ClassLoader) extends Deserializers.Base {
  override def findBeanDeserializer(javaType: JavaType, config: DeserializationConfig,
    provider: DeserializerProvider, beanDesc: BeanDescription,
    property: BeanProperty) = {
    val klass = javaType.getRawClass
    if (classOf[JsValue].isAssignableFrom(klass) || klass == JsNull.getClass) {
      new JsValueDeserializer(config.getTypeFactory, klass)
    } else null
  }

}

private[json] class PlaySerializers extends Serializers.Base {
  override def findSerializer(config: SerializationConfig, javaType: JavaType, beanDesc: BeanDescription, beanProp: BeanProperty) = {
    val ser: Object = if (classOf[JsValue].isAssignableFrom(beanDesc.getBeanClass)) {
      new JsValueSerializer
    } else {
      null
    }
    ser.asInstanceOf[JsonSerializer[Object]]
  }
}

private[json] object JerksonJson extends com.codahale.jerkson.Json {
  import org.codehaus.jackson.Version
  import org.codehaus.jackson.map.module.SimpleModule
  import org.codehaus.jackson.map.Module.SetupContext

  object module extends SimpleModule("PlayJson", Version.unknownVersion()) {
    override def setupModule(context: SetupContext) {
      context.addDeserializers(new PlayDeserializers(classLoader))
      context.addSerializers(new PlaySerializers)
    }
  }
  mapper.registerModule(module)

}
