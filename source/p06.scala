/*
# P06 (\*) Find out whether a list is a palindrome.

Example:
``` scala
scala> isPalindrome(List(1, 2, 3, 2, 1))
res0: Boolean = true
```
*/

object P06 {
  def isPalindrome[A](l: List[A]): Boolean =
    l == l.reverse
}
