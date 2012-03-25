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
      (Lens.self \ "author" \ "firstname")(article) must equalTo(JsString("Bugs"))
    }

    "lens reads a sub-array" in {
      (Lens.self \ "tags" \ 0)(article) must equalTo(JsString("Awesome article"))
    }

    //"set a value with lenses" in {
    //  import play.api.libs.json.Implicits._
    //  (Json \ "author" \ "firstname").set(article,"Albert") must equalTo(JsString("Acme"))
    //}

//    "mask -whitelist- an object" in {
//      article &&& List("title", "author") > "tags" get must equalTo(JsUndefined)
//    }
//    "mask -blacklist- an object" in {
//      article ||| List("tags") > "tags" get must equalTo(JsUndefined)
//    }
  }

}

// vim: set ts=2 sw=2 ft=scala et:
