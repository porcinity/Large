package models

object Tag:
  opaque type TagId = String
  object TagId:
    def apply(value: String): TagId = value
  extension (x: TagId) def value: String = x

  opaque type TagName = String
  object TagName:
    def apply(value: String): TagName = value

  case class Tag(name: TagName)
