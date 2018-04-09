/*
# P10 (\*) Run-length encoding of a list.

Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.

Example:
``` scala
scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
```
*/

object P10 {
  def encode[A](l: List[A]): List[(Int, A)] =
    pack(l) map (a => (a.length, a.head))
    
  def encode_2[A](l: List[A]): List[(Int, A)] =
    l.foldRight(List[(Int, A)]())((a, z) => z match {
      case h :: t => if(h._2 == a) (h._1 + 1, h._2) :: t else (1, a) :: z
      case _ => (1, a) :: z
    })

  // P09
  def pack[A](l: List[A]): List[List[A]] =
    l.foldRight(Nil: List[List[A]])((a, z) => z match {
      case h :: t => if (h.head == a) (a :: h) :: t else List(a) :: z
      case _ => List(a) :: z
    })
}
