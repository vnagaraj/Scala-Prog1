
object FunSetsTest{
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): Set = x => x == elem


  def union(s: Set, t: Set): Set = {
    def union_check(elem: Int): Boolean = {
      if (s(elem) || t(elem))true
      else false
    }
    union_check
  }


  val s1 = singletonSet(1)
  s1(2)
  val s2 = singletonSet(2)
  val s = union(s1, s2)
  s(-1)



}