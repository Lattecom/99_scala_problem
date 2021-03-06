object P01 {
  // pattern matching version
  def last[A](la: List[A]): A = la match {
    case h :: Nil => h 
    case h :: t => last(t)
  }

  // foldLeft version
  def last_[A](la: List[A]): A = 
    la.foldLeft(la.head)((acc, a) => if (a == la.head) acc else a)
}
