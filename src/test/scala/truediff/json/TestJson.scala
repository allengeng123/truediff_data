package truediff.json

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truediff.diffable.Diffable

class TestJson extends AnyFlatSpec with Matchers {

  def compareAndApply(src: Diffable, dest: Diffable): Unit = {
    println("Comparing:")
    println(s"  $src")
    println(s"  $dest")

    val (changeset,newtree) = src.compareTo(dest)
    println("Changeset:")
    changeset.cmds.foreach(c => println("  " + c))
    println("New tree:")
    println("  " + newtree.toStringWithURI)
    println()

    assertResult(dest)(newtree)
    assertResult(None)(changeset.welltyped)
//    assertResult(expectedChanges)(changeset.size)
    newtree.foreachDiffable(t => assert(t.share == null, s", share of ${t.toStringWithURI} was not reset"))
    newtree.foreachDiffable(t => assert(t.assigned == null, s", assigned of ${t.toStringWithURI} was not reset"))
  }

  import Parser.parse

  val doc1: String = """
    |{
    |  "firstName": "John",
    |  "lastName": "Smith",
    |  "age": 25,
    |  "address": {
    |      "streetAddress": "21 2nd Street",
    |      "city": "New York",
    |      "state": "NY",
    |      "postalCode": 10021
    |  },
    |  "phoneNumbers": [
    |      {
    |          "type": "home",
    |          "number": "212 555-1234"
    |      },
    |      {
    |          "type": "fax",
    |          "number": "646 555-4567"
    |      }
    |  ]
    |}
    |""".stripMargin
  // changed streetAddress, postcode, and fax number
  val doc2: String = """
    |{
    |  "firstName": "John",
    |  "lastName": "Smith",
    |  "age": 25,
    |  "address": {
    |      "streetAddress": "Main Street",
    |      "city": "New York",
    |      "state": "NY",
    |      "postalCode": 10059
    |  },
    |  "phoneNumbers": [
    |      {
    |          "type": "home",
    |          "number": "212 555-1234"
    |      },
    |      {
    |          "type": "fax",
    |          "number": "646 555-4569"
    |      }
    |  ]
    |}
    |""".stripMargin
  // added country, second address
  val doc3: String = """
    |{
    |  "firstName": "John",
    |  "lastName": "Smith",
    |  "age": 25,
    |  "addresses": [
    |    { "address": {
    |        "streetAddress": "Main Street",
    |        "city": "New York",
    |        "state": "NY",
    |        "postalCode": 10059,
    |        "country": "USA"
    |    } },
    |    { "address": {
    |        "streetAddress": "Oxford Lane",
    |        "city": "London",
    |        "postalCode": 99188,
    |        "country": "United Kingdom"
    |    } }
    |  ],
    |  "phoneNumbers": [
    |      {
    |          "type": "home",
    |          "number": "212 555-1234"
    |      },
    |      {
    |          "type": "fax",
    |          "number": "646 555-4569"
    |      }
    |  ]
    |}
    |""".stripMargin
  // reorder stuff
  val doc4: String = """
                       |{
                       |  "lastName": "Smith",
                       |  "firstName": "John",
                       |  "age": 25,
                       |  "addresses": [
                       |    { "address": {
                       |        "streetAddress": "Oxford Lane",
                       |        "city": "London",
                       |        "postalCode": 99188,
                       |        "country": "United Kingdom"
                       |    } },
                       |    { "address": {
                       |        "streetAddress": "Main Street",
                       |        "city": "New York",
                       |        "state": "NY",
                       |        "postalCode": 10059,
                       |        "country": "USA"
                       |    } }
                       |  ],
                       |  "phoneNumbers": [
                       |      {
                       |          "number": "212 555-1234",
                       |          "type": "home"
                       |      },
                       |      {
                       |          "number": "646 555-4569",
                       |          "type": "fax"
                       |      }
                       |  ]
                       |}
                       |""".stripMargin
  "json diff" should "work" in {
    compareAndApply(parse(doc1), parse(doc1))
    compareAndApply(parse(doc1), parse(doc2))
    compareAndApply(parse(doc2), parse(doc3))
    compareAndApply(parse(doc3), parse(doc4))
  }


}
