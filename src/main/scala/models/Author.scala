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
import cats.data.Validated.*
import cats.implicits.*

object Author:

  object Codecs:
    implicit val dtoCodec: Codec[AuthorDto] = deriveCodec[AuthorDto]
    implicit val authorCodec: Codec[Author] = deriveCodec[Author]
    implicit val authorRead: Read[Author] =
      Read[(String, String, String, String, LocalDate)].map { case (id, name, emailAdd, verified, date) =>
        val email = Email(EmailAddress(emailAdd), EmailStatus.fromString(verified))
        Author(AuthorId(id), Name(name), email, date)
      }
    implicit val authorWrite: Write[Author] =
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

  opaque type Age = Int

  object Age:
    def apply(value: Int): Age = value
    def create(value: Int): ValidationResult[Age] = value match
      case x if x > 100 => "Too old".invalidNec
      case _ => value.validNec

    extension (x: Age) def value: Int = x

  opaque type Weight = Int

  object Weight:
    def apply(value: Int): Weight = value

  opaque type AuthorId = String

  object AuthorId:
    def apply(value: String): AuthorId = value
    def create(value: String): ValidationResult[AuthorId] = value.validNec
    def value(authorId: AuthorId): String = authorId
  extension (x: AuthorId) {
    @targetName("value_AuthorId")
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
      case _ => validNec(date)
  extension (x: JoinDate) def value: LocalDate = x

  case class Author(id: AuthorId, name: Name, email: Email, joinDate: JoinDate)

  case class AuthorDto(name: String, email: String)


  object AuthorDto:
    def toDomain(dto: AuthorDto): ValidationResult[Author] =
      val id = NanoIdUtils.randomNanoId()
      val name = Name.create(dto.name)
      val emailAddress = EmailAddress.create(dto.email)
      val email = (emailAddress, EmailStatus.init).mapN(Email.apply)

      (AuthorId.create(id),
        Name.create(dto.name),
        email,
        JoinDate.create(LocalDate.now())).mapN(Author)
//      val monadicc = for {
//        n <- name
//      } yield Author(
//        AuthorId(id),
//        n,
//        Email(
//          EmailAddress(dto.email),
//          EmailStatus.Unverified
//        ),
//        JoinDate(LocalDate.now())
//      )