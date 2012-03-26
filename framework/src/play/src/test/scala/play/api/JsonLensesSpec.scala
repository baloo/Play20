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
      Lens.self(article) must equalTo(article)
    }

    "lens reads a sub-object" in {
      (Lens \ "author" \ "firstname")(article) must equalTo(JsString("Bugs"))
    }

    "lens reads a sub-array" in {
      (Lens \ "tags" at 0)(article) must equalTo(JsString("Awesome article"))
    }

    "compose lenses" in {
      val first = Lens \ "author"
      val second = Lens \ "firstname"

      (second + first)(article) must equalTo(JsString("Bugs"))
    }

    "compose lenses and modify" in {
      val first = Lens \ "author"
      val second = Lens \ "firstname"

      println((second + first)(article, JsString("Daffy")))

      (second + first)(article, JsString("Daffy")) must equalTo(JsObject(
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
      (Lens \ "author" \ "firstname").set(article, "Daffy") must 
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
      (Lens \ "title" \ "foo")(article, "bar") must equalTo(JsObject(
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

//    "mask -whitelist- an object" in {
//      article &&& List("title", "author") > "tags" get must equalTo(JsUndefined)
//    }
//    "mask -blacklist- an object" in {
//      article ||| List("tags") > "tags" get must equalTo(JsUndefined)
//    }
  }

}

// vim: set ts=2 sw=2 ft=scala et:
