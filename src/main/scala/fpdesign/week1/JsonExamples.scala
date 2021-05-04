package fpdesign.week1



object JsonExamples {
  val data = JObj(Map(
    "firstName" -> JStr("John"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("21 2nd Street"),
      "state" -> JStr("NY"),
      "postalCode" -> JNum(10021)
    )),
    "phoneNumbers" -> JSeq(List(
      JObj(Map(
        "type" -> JStr("home"), "number" -> JStr("212 555-1234")
      )),
      JObj(Map(
        "type" -> JStr("fax"), "number" -> JStr("646 555-4567")
      ))
    ))
  ))

  def show(json: JSON): String = json match {
    case JSeq(elems) =>
      "[" + (elems map show mkString ", ") + "]"
    case JObj(bindings) =>
      val assocs = bindings map {
        case (key, value) => "\"" + key + "\": " + show(value)
      }
      "{" + (assocs mkString ", ") + "}"
    case JNum(num) => num.toString
    case JStr(str) => "\"" + str + "\""
    case JBool(b) => b.toString
    case JNull => "null"
  }

  def patternMatchingInFor(paramData: List[JSON]) =
    for {
      JObj(bindings) <- paramData
      JSeq(phones) = bindings("phoneNumbers")
      JObj(phone) <- phones
      JStr(digits) = phone("number")
      if digits startsWith "212"
    } yield (bindings("firstName"), bindings("lastName"))

  def main(args: Array[String]): Unit = {
    println(show(data))
    println(patternMatchingInFor(List(data)))
  }
}
