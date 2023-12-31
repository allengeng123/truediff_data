package truediff.consumers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truediff.Diffable
import truediff.macros._

class ParentNoListsTests extends AnyFlatSpec with Matchers  {

  "parents" should "maintain correct parent" in {
    val parent = new ParentNoLists


    val tree1 = Add(Add(Num(1), Num(2)), Add(Num(3), Num(4)))
    val (editscript1) = Diffable.load(tree1)
    val n1 = tree1.e1.asInstanceOf[Add].e1.uri
    val n2 = tree1.e1.asInstanceOf[Add].e2.uri
    val n3 = tree1.e2.asInstanceOf[Add].e1.uri
    val n4 = tree1.e2.asInstanceOf[Add].e2.uri

    parent.update(editscript1)

    assert(parent.parents.size == tree1.treesize)
    assert(parent(n1).isDefined)
    assert(parent(n2) == parent(n1))
    assert(parent(n3).isDefined)
    assert(parent(n4) == parent(n3))
    assert(parent(tree1.uri) == Some(null))



    val (editscript2, tree2) = tree1.compareTo(Add(Add(Num(4), Num(2)), Add(Num(3), Num(1))))
    parent.update(editscript2)
    assert(parent.parents.size == tree2.treesize)
    assert(parent(n1).isDefined)
    assert(parent(n2) == parent(n1))
    assert(parent(n3).isDefined)
    assert(parent(n4) == parent(n3))
    assert(parent(tree2.uri) == Some(null))



    val (editscript3, tree3) = tree2.compareTo(Add(Add(Num(4), Add(Num(2), Num(5))), Add(Num(3), Num(1))))
    parent.update(editscript3)
    assert(parent.parents.size == tree3.treesize)
    assert(parent(n4).isDefined)
    assert(parent(n2) != parent(n4))
    assert(parent(n3).isDefined)
    assert(parent(n1) == parent(n2))
    assert(parent(tree3.uri) == Some(null))

  }

}
