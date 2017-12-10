object P01 {
  def last[A](la: List[A]): A = la match {
    case h :: Nil => h 
    case h :: t => last(t)
  }

  def last_[A](la: List[A]): A = 
    la.foldRight(la.head)((acc, a) => if (a == la.head) acc else a)
}
