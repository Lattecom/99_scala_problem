/*
# P02(\*): Find the *K*th element of a list.

Example:
```scala
scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
```
*/

object P03 {
  // tailrec & pattern matching version
  def nth[A](n: Int, la: List[A]): A = la match {
    case _ :: t if(n > 0) => nth(n-1, t)
    case h :: _ if(n == 0) => h
    case Nil => throw new NoSuchElementException
  }
}
