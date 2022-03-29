package models

object ValidationExtractors:
  object FewerThan5:
    def unapply(x: String): Boolean = x.length < 5 && x.nonEmpty

  object Over100:
    def unapply(x: String): Boolean = x.length > 100

  object Betwen5and100:
    def unapply(x: String): Boolean = ???
    
  object Over150:
    def unapply(x: String): Boolean = x.length > 150
    
  object Over15k:
    def unapply(x: String): Boolean = x.length > 15000

  object NumbersOrChars:
    def unapply(x: String): Boolean = !x.matches("^[a-zA-Z]+$")

  object EmptyName:
    def unapply(x: String): Boolean = x.isEmpty
