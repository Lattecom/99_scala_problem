/*
# P14 (\*) Duplicate the elements of a list.

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
    l.foldRight(List(): List[A])((a, z) => List(a,a) ::: z)
}
