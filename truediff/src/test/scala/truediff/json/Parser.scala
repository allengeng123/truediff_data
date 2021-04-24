package truediff.json

// adapted from https://github.com/lihaoyi/fastparse/blob/master/fastparse/test/src/fastparse/JsonTests.scala

object Parser {
  import fastparse._
  import NoWhitespace._

  def parse(s: String): Js = fastparse.parse(s, Parser.jsonExpr(_)).get.value

  def stringChars(c: Char) = c != '\"' && c != '\\'

  def space[_: P]         = P( CharsWhileIn(" \r\n", 0) )
  def digits[_: P]        = P( CharsWhileIn("0-9") )
  def exponent[_: P]      = P( CharIn("eE") ~ CharIn("+\\-").? ~ digits )
  def fractional[_: P]    = P( "." ~ digits )
  def integral[_: P]      = P( "0" | CharIn("1-9")  ~ digits.? )

  def number[_: P] = P(  CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => Js.Num(x.toDouble)
  )

  def `null`[_: P]        = P( "null" ).map(_ => Js.Null())
  def `false`[_: P]       = P( "false" ).map(_ => Js.False())
  def `true`[_: P]        = P( "true" ).map(_ => Js.True())

  def hexDigit[_: P]      = P( CharIn("0-9a-fA-F") )
  def unicodeEscape[_: P] = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  def escape[_: P]        = P( "\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape) )

  def strChars[_: P] = P( CharsWhile(stringChars) )
  def string[_: P] =
    P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str.apply)

  def array[_: P] =
    P( "[" ~/ jsonExpr.rep(sep=","./) ~ space ~ "]").map(Js.Arr.apply)

  def pair[_: P] = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

  def obj[_: P] =
    P( "{" ~/ pair.rep(sep=","./) ~ space ~ "}").map(vs => Js.Obj(vs.map(p => Field(p._1, p._2))))

  def jsonExpr[_: P]: P[Js] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )
}
