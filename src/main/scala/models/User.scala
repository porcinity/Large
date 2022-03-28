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

import org.latestbit.circe.adt.codec._
object User:

  object Codecs:
    implicit val dtoCodec: Codec[UserDto] = deriveCodec[UserDto]
    implicit val userCodec: Codec[User] = deriveCodec[User]
    implicit val userRespCodec: Codec[SingleUser] = deriveCodec[SingleUser]
    implicit val listResCodec: Codec[ListOfUsers] = deriveCodec
    implicit val validationCodec: Codec[ErrorResponse] = deriveCodec

  case class SingleUser(data: User)
  case class ListOfUsers(data: List[User])
  case class ErrorResponse(status: HttpStatus, message: String, errors: Option[List[String]])

  enum HttpStatus derives JsonTaggedAdt.PureEncoder, JsonTaggedAdt.PureDecoder:
    case Ok
    case UnprocessableEntity
    case NotFound

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