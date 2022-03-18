package models

import doobie.Read
import io.circe.{Codec, Decoder}
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}

object Author:
  implicit val authorCodec: Codec[Author] = deriveCodec[Author]
//  implicit val emailCodec: Decoder[Email] = deriveDecoder[Email]
  implicit val authorRead: Read[Author] =
    Read[(Int, String, String)].map { case (id, name, email) =>
//      val status = VerificationStatus.fromString(emailStatus)
      // , Email(EmailAddress(emailAddress), EmailVerification(emailStatus))
//      val email = Email(EmailAddress(emailAdd), EmailVerification(verified))
      Author(AuthorId(id), Name(name), EmailAddress(email))
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

  opaque type EmailVerification = String
  object EmailVerification:
    def apply(value: String): EmailVerification = value

  enum VerificationStatus:
    case Verified
    case Unverified

  object VerificationStatus:
    def fromString(string: String): VerificationStatus = string match
      case "Unverified" => VerificationStatus.Unverified
      case "Verified" => VerificationStatus.Verified

  case class Email(address: EmailAddress, verified: EmailVerification)

  case class Author(id: AuthorId, name: Name, emailAddress: EmailAddress)
