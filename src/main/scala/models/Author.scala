package models

import doobie.Read
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}

import io.circe.syntax.*
import io.circe.generic.auto.*

object Author:
  implicit val authorCodec: Codec[Author] = deriveCodec[Author]
  implicit val emailCodec: Codec[Email] = deriveCodec[Email]
//  implicit val statusCodec: Codec[VerificationStatus] = deriveCodec[VerificationStatus]
  implicit val statusDecode: Encoder[VerificationStatus] = Encoder.instance {
    case verified @ Verified() => verified.asJson
    case unverified @ Unverified(_) => unverified.asJson
  }
  implicit val authorRead: Read[Author] =
    Read[(Int, String, String, String)].map { case (id, name, emailAdd, verified) =>
      val status = EmailVerification.make(verified)
      val hmm = EmailVerification.cake("")
      val email = Email(EmailAddress(emailAdd), status)
      Author(AuthorId(id), Name(name), email)
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
    def make(input: String): EmailVerification = input match {
      case "Verified" => new EmailVerification("Verified")
      case _ => new EmailVerification("Unverified")
    }
    val cake: String => EmailVerification = {
      case "verified" => new EmailVerification("Verified")
      case _ => new EmailVerification("Unverified")
    }

//  enum VerificationStatus:
//    case Verified
//    case Unverified

  sealed trait VerificationStatus
  case class Unverified(uv: String) extends VerificationStatus
  case class Verified() extends VerificationStatus

  object VerificationStatus:
    def fromString(string: String): VerificationStatus = string match
      case "Unverified" => Unverified("Unverified")
      case "Verified" => Verified()

  case class Email(address: EmailAddress, emailStatus: EmailVerification)

  case class Author(id: AuthorId, name: Name, email: Email)
