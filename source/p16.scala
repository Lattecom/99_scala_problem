/*
# P16 (\*\*) Drop every *N*th element from a list

Example:
``` scala
scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
```
*/

object P16 {
  def drop[A](n: Int, la: List[A]): List[A] =
    la.zipWithIndex.foldRight(List[A]()) {
      (a, z) => {
        if (a._2 % n == 2) z
        else a._1 :: z
      }
    }
}
