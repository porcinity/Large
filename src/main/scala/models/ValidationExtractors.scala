package models

object ValidationExtractors:

  object From2to30Chars:
    def unapply(x: String): Boolean = x.matches("^[a-z A-z]{2,30}+$")
    
  object ValidEmail:
    def unapply(x: String): Boolean = x.matches("""^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""")

  object FewerThan5:
    def unapply(x: String): Boolean = x.length < 5 && x.nonEmpty

  object Over100:
    def unapply(x: String): Boolean = x.length > 100
    
  object LessOrEqual150:
    def unapply(x: String): Boolean = 0 < x.length && x.length <= 150
    
  object LessOrEqual15k:
    def unapply(x: String): Boolean = 0 < x.length && x.length <= 15000

  object NumbersOrChars:
    def unapply(x: String): Boolean = !x.matches("^[a-zA-Z]+$")

  object EmptyName:
    def unapply(x: String): Boolean = x.isEmpty
