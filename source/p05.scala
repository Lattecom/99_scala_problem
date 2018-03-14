/*
# P05 (\*) Reverse a list.

Example:
``` scala
scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
```
*/

object P05 {
  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(originList: List[A], revList: List[A]): List[A] = originList match {
      case h :: t => go(t, h :: revList)
      case Nil => revList
    }
    go(l, List[A]())
  }

  def reverseViaFoldLeft[A](l: List[A]): List[A] =
    l.foldLeft(List[A]())((z, a) =>  a :: z)
}
