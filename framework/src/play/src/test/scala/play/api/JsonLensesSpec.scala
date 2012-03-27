package play.api.libs.json

import org.specs2.mutable._

object JsonLensesSpec extends Specification {
  val article = JsObject(
    List(
      "title" -> JsString("Acme"),
      "author" -> JsObject(
        List(
          "firstname" -> JsString("Bugs"),
          "lastname" -> JsString("Bunny")
          )
        ),
      "tags" -> JsArray(
        List[JsValue](
          JsString("Awesome article"),
          JsString("Must read"),
          JsString("Playframework"),
          JsString("Rocks")
          )
        )
      )
    )

  "JSONLenses" should {
    "lens.self returns identity" in {
      JsValue(article) must equalTo(article)
    }

    "lens reads a sub-object" in {
      (JsValue \ "author" \ "firstname")(article) must equalTo(JsString("Bugs"))
    }

    "lens reads a sub-object with as" in {
      ((JsValue \ "author" \ "firstname").as[String]).get(article) must equalTo("Bugs")
    }

    "lens sets a sub-object with as" in {
      ((JsValue \ "author" \ "firstname").as[String]).set(article,"Koko") must equalTo(JsObject(
    List(
      "title" -> JsString("Acme"),
      "author" -> JsObject(
        List(
          "firstname" -> JsString("Koko"),
          "lastname" -> JsString("Bunny")
          )
        ),
      "tags" -> JsArray(
        List[JsValue](
          JsString("Awesome article"),
          JsString("Must read"),
          JsString("Playframework"),
          JsString("Rocks")
          )
        )
      )
    ))
    }

    "lens outside a sub-object with as throws exceptions" in {
      ((JsValue \ "author" \ "non-existant").as[String]).get(article) must throwAn[RuntimeException]
    }

    "lens outside a sub-object with asEither returns Left('some error')" in {
      ((JsValue \ "author" \ "non-existant").asEither[String]).get(article) must
        equalTo(Left[String, String]("Field non-existant not found"))
    }

    "lens sets a sub-object with asEither with Right(String)" in {
      (((JsValue \ "author" \ "login")
        .asEither[String]).set(article, Right[String,String]("bbunny"))) must
        equalTo(JsObject(
          List(
            "title" -> JsString("Acme"),
            "author" -> JsObject(
              List(
                "firstname" -> JsString("Bugs"),
                "lastname" -> JsString("Bunny"),
                "login" -> JsString("bbunny")
                )
              ),
            "tags" -> JsArray(
              List[JsValue](
                JsString("Awesome article"),
                JsString("Must read"),
                JsString("Playframework"),
                JsString("Rocks")
                )
              )
            )
          ))
    }

    "lens outside a sub-object with asOpt returns None" in {
      ((JsValue \ "author" \ "non-existant").asOpt[String]).get(article) must equalTo(None)
    }

    "lens sets a sub-object with asOpt with Some(String)" in {
      ((JsValue \ "author" \ "firstname").asOpt[String]).set(article,Some("Elmer")) must equalTo(JsObject(
    List(
      "title" -> JsString("Acme"),
      "author" -> JsObject(
        List(
          "firstname" -> JsString("Elmer"),
          "lastname" -> JsString("Bunny")
          )
        ),
      "tags" -> JsArray(
        List[JsValue](
          JsString("Awesome article"),
          JsString("Must read"),
          JsString("Playframework"),
          JsString("Rocks")
          )
        )
      )
    ))
    }

    "lens reads a sub-array" in {
      (JsValue \ "tags" at 0)(article) must equalTo(JsString("Awesome article"))
    }

    "compose lenses" in {
      val first =  JsValue \ "author"
      val second = JsValue \ "firstname"

      (first andThen second).apply(article) must equalTo(JsString("Bugs"))
    }

    "compose lenses and modify" in {
      val first =  JsValue \ "author"
      val second = JsValue \ "firstname"

      (first andThen second).apply(article, JsString("Daffy")) must equalTo(JsObject(
          List(
            "title" -> JsString("Acme"),
            "author" -> JsObject(
              List(
                "firstname" -> JsString("Daffy"),
                "lastname" -> JsString("Bunny")
                )
              ),
            "tags" -> JsArray(
              List[JsValue](
                JsString("Awesome article"),
                JsString("Must read"),
                JsString("Playframework"),
                JsString("Rocks")
                )
              )
            )
          )
        )
 
    }


    //"set a value with lenses" in {
    //  import play.api.libs.json.Implicits._
    //  (Lens.self \ "title")((Lens.self \ "title").set(article,"Acme More")) must 
    //    equalTo(JsString("Acme More"))
    //}

    "set a value in a subobject with lenses" in {
      import play.api.libs.json.Implicits._
      (JsValue \ "author" \ "firstname").set(article, "Daffy") must 
        equalTo(JsObject(
          List(
            "title" -> JsString("Acme"),
            "author" -> JsObject(
              List(
                "firstname" -> JsString("Daffy"),
                "lastname" -> JsString("Bunny")
                )
              ),
            "tags" -> JsArray(
              List[JsValue](
                JsString("Awesome article"),
                JsString("Must read"),
                JsString("Playframework"),
                JsString("Rocks")
                )
              )
            )
          )
        )
    }

    "set a value outside of an object" in {
      import play.api.libs.json.Implicits._
      (JsValue \ "title" \ "foo")(article, "bar") must equalTo(JsObject(
        List(
          "title" -> JsObject(
            List(
              "foo" -> JsString("bar")
              )
            ),
          "author" -> JsObject(
            List(
              "firstname" -> JsString("Bugs"),
              "lastname" -> JsString("Bunny")
              )
            ),
          "tags" -> JsArray(
            List[JsValue](
              JsString("Awesome article"),
              JsString("Must read"),
              JsString("Playframework"),
              JsString("Rocks")
              )
            )
          )
        )
      )
    }

    "selectAll strings in article" in {
      JsValueLens.init
      .selectAll(article, a => a match {
        case JsString(_) => true
        case _ => false
        })
       .map(t => t._2) must equalTo(Seq(
         "Acme", "Bugs", "Bunny", "Awesome article", "Must read",
         "Playframework", "Rocks"
       ).map(t => JsString(t)))
    }

    "suffix all strings with a space" in {
      JsValueLens.init \\ (article, a => a match {
        case JsString(_) => true
        case _ => false
        }, a => a match {
          case JsString(s) => JsString(s + " ")
          case o => o
        }) must equalTo(JsObject(
          List(
            "title" -> JsString("Acme "),
            "author" -> JsObject(
              List(
                "firstname" -> JsString("Bugs "),
                "lastname" -> JsString("Bunny ")
                )
              ),
            "tags" -> JsArray(
              List[JsValue](
                JsString("Awesome article "),
                JsString("Must read "),
                JsString("Playframework "),
                JsString("Rocks ")
                )
              )
            )
          )
        )
    }

//    "mask -whitelist- an object" in {
//      article &&& List("title", "author") > "tags" get must equalTo(JsUndefined)
//    }
//    "mask -blacklist- an object" in {
//      article ||| List("tags") > "tags" get must equalTo(JsUndefined)
//    }
  }

}

// vim: set ts=2 sw=2 ft=scala et:
