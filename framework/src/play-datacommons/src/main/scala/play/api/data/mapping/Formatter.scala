package play.api.data.mapping

trait From[I] {
	def apply[O](f: Reader[I] => Rule[I, O]) = f(Reader[I]())
}
object From { def apply[I] = new From[I]{} }

trait To[I] {
	def apply[O](f: Writer[I] => Write[O, I]) = f(Writer[I]())
}
object To { def apply[I] = new To[I]{} }

case class Reader[I](path: Path = Path(Nil)) {
  /**
  * When applied, the rule will lookup for data at the given path, and apply the `sub` Rule on it
  * {{{
  *   val json = Json.parse("""{
  *      "informations": {
  *        "label": "test"
  *      }
  *   }""")
  *   val infoValidation = From[JsValue]{ __ => (__ \ "label").read(nonEmptyText) }
  *   val v = From[JsValue]{ __ => (__ \ "informations").read(infoValidation)) }
  *   v.validate(json) == Success("test")
  * }}}
  * @param sub the constraint to apply on the subdata
  * @param l a lookup function. This function finds data in a structure of type I, and coerce it to type O
  * @return A Rule validating the existence and validity of data at `path`
  */
  def read[J, O](sub: Rule[J, O])(implicit r: Path => Rule[I, J]): Rule[I, O] =
    r(path).compose(path)(sub)


  /**
  * Try to convert the data at `Path` to type `O`
  * {{{
  *   val json = Json.parse("""{
  *      "informations": {
  *        "label": "test"
  *      }
  *   }""")
  *   implicit val infoValidation = From[JsValue]{ __ => (__ \ "label").read[String] }
  *   val v = From[JsValue]{ __ => (__ \ "informations").read[Informations]) }
  *   v.validate(json) == Success("test")
  * }}}
  * @param r a lookup function. This function finds data in a structure of type I, and coerce it to type O
  * @return A Rule validating the existence and validity of data at `path`.
  */
  def read[O](implicit r: Path => Rule[I, O]): Rule[I, O] =
    read(Rule.zero[O])(r)

  def \(key: String): Reader[I] = Reader(path \ key)
  def \(idx: Int): Reader[I] = Reader(path \ idx)
  def \(child: PathNode): Reader[I] = Reader(path \ child)
}

case class Writer[I](path: Path = Path(Nil)) {
   /**
  * Create a Write that convert data to type `I`, and put it at Path `path`
  * {{{
  *   val w = To[JsObject] { __ =>
  *      (__ \ "informations").write[Seq[String]])
  *   }
  *   w.writes(Seq("foo", "bar")) == Json.obj("informations" -> Seq("foo", "bar"))
  * }}}
  * @note This method works fine with recursive writes
  */
  def write[O](implicit w: Path => Write[O, I]): Write[O, I] =
    Write{ x => w(path).writes(x) } // makes it lazy evaluated. Allows recursive writes

  /**
  * Create a Write that convert data to type `I`, and put it at Path `path`
  * {{{
  *   val w = To[JsObject] { __ =>
  *      (__ \ "date").write(date("yyyy-MM-dd""))
  *   }
  *   w.writes(new Date()) == Json.obj("date" -> "2013-10-3")
  * }}}
  * @note This method works fine with recursive writes
  */
  def write[O, J](format: => Write[O, J])(implicit w: Path => Write[J, I]): Write[O, I] =
    w(path).contramap(x => format.writes(x))

  def \(key: String): Writer[I] = Writer(path \ key)
  def \(idx: Int): Writer[I] = Writer(path \ idx)
  def \(child: PathNode): Writer[I] = Writer(path \ child)
}