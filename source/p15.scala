/*
# P15 (\*\*) Duplicate the elements of a list a given number of times.

Example:
``` scala
scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
```
*/

object P15 {
  def duplicateN[A](n: Int, l: List[A]): List[A] = 
    l.flatMap(a => List.fill(n)(a))

  def duplicateNViaFoldRight[A](n: Int, l: List[A]): List[A] = 
    l.foldRight(List(): List[A])((a, z) => List.fill(n)(a) ::: z)
}
