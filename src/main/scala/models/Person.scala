package models

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

object Person:
  implicit val personCodec: Codec[Person] = deriveCodec[Person]
  opaque type Name = String

  object Name:
    def apply(value: String): Name = value
    extension (x: Name)
      def value: String = x

  opaque type Age = Int

  object Age:
    def apply(value: Int): Age = value

    extension (x: Age)
      def value: Int = x

  opaque type Weight = Int

  object Weight:
    def apply(value: Int): Weight = value

  opaque type PersonId = Int
  object PersonId:
    def apply(value: Int): PersonId = value
  extension (x: PersonId)
    def value: Int = x

  case class Person(id: PersonId, name: Name, age: Age, weight: Weight)
