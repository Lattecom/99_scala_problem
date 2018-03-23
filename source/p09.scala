/*
# P09 (\*\*) Pack consecutive duplicates of list elements into sublists.

If a list contains repeated elements they should be placed in separate sublists.

Example:
``` scala
scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
```
*/

object P09 {
  def pack[A](l: List[A]): List[List[A]] =
    l.foldRight(Nil: List[List[A]])((a, z) => z match {
      case h :: t => if (h.head == a) (a :: h) :: t else List(a) :: z
      case _ => List(a) :: z
    })
}

