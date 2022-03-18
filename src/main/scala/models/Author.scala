package models

import doobie.Read
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

object Author:
  implicit val authorCodec: Codec[Author] = deriveCodec[Author]
  implicit val authorRead: Read[Author] =
    Read[(Int, String, String, String)].map { case (id, name, emailAddress, emailStatus) =>
      val status = VerificationStatus.fromString(emailStatus)
      Author(AuthorId(id), Name(name), Email(EmailAddress(emailAddress), status))
    }

  opaque type Name = String

  object Name:
    def apply(value: String): Name = value
    extension (x: Name)
      def value: String = x

  opaque type Age = Int

  object Age:
    def apply(value: Int): Age = value

    extension (x: Age) def value: Int = x

  opaque type Weight = Int

  object Weight:
    def apply(value: Int): Weight = value

  opaque type AuthorId = Int

  object AuthorId:
    def apply(value: Int): AuthorId = value
  extension (x: AuthorId) def value: Int = x

  opaque type EmailAddress = String
  object EmailAddress:
    def apply(value: String): EmailAddress = value

  enum VerificationStatus:
    case Verified
    case Unverified
    
  object VerificationStatus:
    def fromString(string: String): VerificationStatus = string match
      case "Unverified" => VerificationStatus.Unverified
      case "Verified" => VerificationStatus.Verified

  case class Email(address: EmailAddress, verified: VerificationStatus)

  case class Author(id: AuthorId, name: Name, email: Email)
