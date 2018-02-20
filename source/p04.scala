/*
# P04 (\*) Find the number of elements of a list.

Example:
``` scala
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
```
*/

object P04 {
    def length[A](l: List[A]): Int = {
        @annotation.tailrec
        def go(la: List[A], acc: Int): Int = la match {
            case Nil => acc
            case _ :: t => go(t, acc+1)
        }
        go(l, 0)
    }
    
    def lengthViaFoldRight[A](l: List[A]): Int =
        l.foldRight(0)((_, x) => x+1)
    
    def lengthViaFoldLeft[A](l: List[A]): Int =
        l.foldLeft(0)((x, _) => x +1)
}