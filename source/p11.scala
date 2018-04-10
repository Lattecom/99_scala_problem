/*
# P11 (\*) Modified run-length encoding.

Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.

Example:
``` scala
scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
```
*/

object P10 {

  def encodeModified[A](l: List[A]): List[Any] =
    encode(l) map (a => if (a._1 == 1) a._2 else a)

  def encodeModified_2[A](l: List[A]): List[Any] =
    l.foldRight(List[Any]())((a, z) => z match {
      case h :: t => {
        h match {
          case (number: Int, value) => if(value == a) (number+1, value) :: t else a :: z
          case value => if(value == a) (2, value) :: t else a :: z
        }
      }
      case _ => a :: z
    })

  // P10
  def encode[A](l: List[A]): List[(Int, A)] =
    pack(l) map (a => (a.length, a.head))

  // P09
  def pack[A](l: List[A]): List[List[A]] =
    l.foldRight(Nil: List[List[A]])((a, z) => z match {
      case h :: t => if (h.head == a) (a :: h) :: t else List(a) :: z
      case _ => List(a) :: z
    })
}
