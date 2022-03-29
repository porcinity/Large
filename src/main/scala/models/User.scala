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
import org.latestbit.circe.adt.codec.*

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

  case class User(id: UserId, name: Name, email: Email, joinDate: JoinDate)

  opaque type Name = String

  object Name:
    def apply(value: String): Name = value
    def create(value: String): ValidationResult[Name] =
      val trimmed = value.trim
      trimmed match
        case From2to30Chars() => trimmed.validNec
        case _ => "Name must be from 2 to 30 characters, letters only.".invalidNec
    extension (x: Name) def value: String = x

  opaque type UserId = String

  object UserId:
    def apply(value: String): UserId = value
    def create(value: String): ValidationResult[UserId] = value.validNec
    extension (x: UserId) {
      @targetName("value_UserId")
      def value: String = x
  }

  opaque type EmailAddress = String

  object EmailAddress:
    def apply(value: String): EmailAddress = value
    def create(value: String): ValidationResult[EmailAddress] = value match
      case ValidEmail() => value.validNec
      case _ => "Please enter a valid email address.".invalidNec
    extension (x: EmailAddress) def value: String = x

  enum EmailStatus derives JsonTaggedAdt.PureEncoder, JsonTaggedAdt.PureDecoder:
    case Verified
    case Unverified

  object EmailStatus:
    def fromString(x: String): EmailStatus = x match
      case "Verified" => EmailStatus.Verified
      case _ => EmailStatus.Unverified
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

  case class UserDto(name: String, email: String)

  object UserDto:
    def toDomain(dto: UserDto): ValidationResult[User] =
      val id = NanoIdUtils.randomNanoId()
      val email = (
        EmailAddress.create(dto.email),
        EmailStatus.init).mapN(Email.apply)

      (
        UserId.create(id),
        Name.create(dto.name),
        email,
        JoinDate.create(LocalDate.now())
        ).mapN(User.apply)