package models

import doobie.{Read, Write}
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}
import io.circe.syntax.*
import io.circe.generic.auto.*

import java.time.LocalDate
import doobie.implicits.legacy.localdate.*
import com.aventrix.jnanoid.jnanoid.*
import models.ValidationExtractors.*

import scala.annotation.targetName

import cats.data.*
import cats.implicits.*

object User:

  object Codecs:
    implicit val dtoCodec: Codec[UserDto] = deriveCodec[UserDto]
    implicit val authorCodec: Codec[User] = deriveCodec[User]
    implicit val authorRead: Read[User] =
      Read[(String, String, String, String, LocalDate)].map { case (id, name, emailAdd, verified, date) =>
        val email = Email(EmailAddress(emailAdd), EmailStatus.fromString(verified))
        User(UserId(id), Name(name), email, date)
      }
    implicit val authorWrite: Write[User] =
      Write[(String, String, String, String, LocalDate)].contramap { author =>
        (
          author.id,
          author.name,
          author.email.address,
          EmailStatus.makeString(author.email.status),
          author.joinDate
        )
      }

  type ValidationError = String
  type ValidationResult[A] = ValidatedNec[ValidationError, A]

  opaque type Name = String

  object Name:
    def apply(value: String): Name = value
    def create(value: String): ValidationResult[Name] = value match
      case EmptyName() => "Name cannot be empty.".invalidNec
      case FewerThan5() => "Name must be longer than 5 characters.".invalidNec
      case Over100() => "Name cannot be longer than 100 characters".invalidNec
      case NumbersOrChars() => "Name cannot contain numbers or special characters.".invalidNec
      case _ => value.validNec
    extension (x: Name)
      def value: String = x

  opaque type UserId = String

  object UserId:
    def apply(value: String): UserId = value
    def create(value: String): ValidationResult[UserId] = value.validNec
    def value(authorId: UserId): String = authorId
  extension (x: UserId) {
    @targetName("value_UserId")
    def value: String = x
  }

  opaque type EmailAddress = String
  object EmailAddress:
    def apply(value: String): EmailAddress = value
    def create(value: String): ValidationResult[EmailAddress] = value match {
      case EmptyName() => "Empty ass lookin ass email".invalidNec
      case FewerThan5() => "Email must be longer than 5 characters.".invalidNec
      case _ => value.validNec
    }
  extension (x: EmailAddress) {
    @targetName("value_EmailAddress")
    def value: String = x
  }

  import org.latestbit.circe.adt.codec._
  enum EmailStatus derives JsonTaggedAdt.PureEncoder, JsonTaggedAdt.PureDecoder:
    case Verified
    case Unverified

  object EmailStatus:
    def fromString(x: String): EmailStatus = x match {
      case "Verified" => EmailStatus.Verified
      case _ => EmailStatus.Unverified
    }
    def makeString(vs: EmailStatus): String = vs match
      case Verified => "Verified"
      case Unverified => "Unverified"
    def init: ValidationResult[EmailStatus] = Unverified.validNec

  case class Email(address: EmailAddress, status: EmailStatus)

  opaque type JoinDate = LocalDate
  object JoinDate:
    def apply(date: LocalDate): JoinDate = date
    def create (date: LocalDate): ValidationResult[JoinDate] = date match
      case _ => date.validNec
  extension (x: JoinDate) def value: LocalDate = x

  case class User(id: UserId, name: Name, email: Email, joinDate: JoinDate)

  case class UserDto(name: String, email: String)


  object UserDto:
    def toDomain(dto: UserDto): ValidationResult[User] =
      val id = NanoIdUtils.randomNanoId()
      val name = Name.create(dto.name)
      val emailAddress = EmailAddress.create(dto.email)
      val email = (emailAddress, EmailStatus.init).mapN(Email.apply)

      (
        UserId.create(id),
        Name.create(dto.name),
        email,
        JoinDate.create(LocalDate.now())
        ).mapN(User.apply)