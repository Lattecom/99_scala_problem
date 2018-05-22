/*
# P14 (\*) Duplicate the elements of a list.

Implement the so-called run-length encoding data compression method directly.
I.e. don't use other methods you've written (like P09's pack); do all the work directly.

Example:
``` scala
scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
```
*/

object P14 {
  def duplicate[A](l: List[A]): List[A] = 
    l.flatMap(a => List(a,a))

  def duplicateViaFoldRight[A](l: List[A]): List[A] = 
    l.foldRight(List[A].empty())((a, z) => List(a,a) ::: z)
}
