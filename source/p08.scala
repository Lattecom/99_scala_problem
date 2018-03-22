/*
# P08 (\*\*) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:
``` scala
scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
```
*/

object P08 {
  def compress[A](l: List[A]): List[A] =
    l.foldRight(Nil: List[A])((a, z) => z match {
      case h :: t => if (h == a) z else a :: z
      case _ => a :: z
    })

  def compress_[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(l: List[A], a: A, acc: List[A]): List[A] = l match {
      case h :: t => if (h == a) go(t, a, acc) else go(t, h, acc ::: List(h))
      case _ => acc
    }

    l match {
      case h :: t => go(t, h, List(h))
      case _ => l
    }
  }
}

