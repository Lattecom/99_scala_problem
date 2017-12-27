object P02 {
  // tailrec & pattern matching version
  def length[A](la: List[A]): Int = {
    @annotation.tailrec
    def go(l: List[A], acc: Int): Int = l match {
      case Nil => acc
      case h :: t => go(t, acc+1)
    }
    go(la,0)
  }

  // foldLeft version
  def length_[A](la: List[A]): Int = 
    la.foldLeft(0)((acc, _) => acc + 1)
}
