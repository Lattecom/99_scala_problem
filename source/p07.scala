/*
# P07 (\*\*) Flatten a nested list structure.

Example:
``` scala
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)
```s
*/

object P07 {
  def flatten(la: List[Any]): List[Any] = 
    la.flatMap{
      case a: List[Any] => flatten(a)
      case _ => List(_)
    }

  def flatten_2(l: List[Any]): List[Any] = l match {
    case (h: List[Any]) :: t => flatten_2(h ::: t)
    case h :: t => h :: flatten_2(t)
    case Nil => Nil
  }

  def flatten_3(l: List[Any]): List[Any] = {
    @annotation.tailrec
    def go(origin: List[Any], acc: List[Any]): List[Any] = origin match {
      case (h: List[Any]) :: t => go(h ::: t, acc)
      case h :: t => go(t, acc ::: List(h))
      case Nil => acc
    }
    go(l, Nil)
  }
}
