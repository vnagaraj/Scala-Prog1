package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("using make code trees") {
    new TestTrees {
      val l1 = Leaf('d', 5)
      val l2 = Leaf('g', 7)
      val f1 = makeCodeTree(l1, l2)
      val l3 = Leaf('h', 8)
      val l4 = Leaf('k', 9)
      val f2 = makeCodeTree(l3, l4)
      val f3 = makeCodeTree(f1, f2)
      assert(weight(f3) === 29)
      assert(chars(f3) === List('d','g','h', 'k'))
    }
  }

  test("times test") {
    new TestTrees {
      val x = times(List('a','b','d','b','a','c','e','d'))
      val y = 2
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val e1 = encode(t1)("ab".toList)
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("e2endTest") {
    new TestTrees {
      val cd = Fork(Leaf('c',1), Leaf('d',1), List('c','d'), 2)
      val ef = Fork(Leaf('e',1), Leaf('f',1), List('e','f'), 2)
      val gh = Fork(Leaf('g',1), Leaf('h',1), List('g','h'), 2)
      val bcd = Fork(Leaf('b',3), cd, List('b','c', 'd'), 5)
      val efgh = Fork(ef, gh, List('e','f', 'g','h'), 4)
      val bcdefgh = Fork(bcd, efgh, List('b', 'c', 'd', 'e' , 'f', 'g','h'), 9)
      val abcdefgh = Fork(Leaf('a',8), bcdefgh, List('a','b', 'c', 'd', 'e' , 'f', 'g','h'), 17)
      val d = decode(abcdefgh, List[Bit](1,0,0,0,1,0,1,0))
      assert(d === "bac".toList)
      val e = encode(abcdefgh)(List('d'))
      assert(e === List[Bit](1,0,1,1))
      val f = quickEncode(abcdefgh)(List('d'))
      assert(f === List[Bit](1,0,1,1))
    }
  }

}
