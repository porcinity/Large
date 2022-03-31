package models

import doobie.{Read, Write}
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}
import io.circe.syntax.*
import io.circe.generic.auto.*

import java.time.LocalDate
import com.aventrix.jnanoid.jnanoid.*
import eu.timepit.refined.boolean.And
//import models.ValidationExtractors.*

import scala.annotation.targetName
import cats.data.*
import cats.implicits.*
import eu.timepit.refined.api.*
import eu.timepit.refined.cats.CatsRefinedTypeOpsSyntax
import eu.timepit.refined.types.string.{NonEmptyFiniteString, NonEmptyString}


import org.latestbit.circe.adt.codec.*
import io.circe.refined._

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

  type UserId = NonEmptyFiniteString[30]
  object UserId extends RefinedTypeOps[NonEmptyFiniteString[30], String] with CatsRefinedTypeOpsSyntax
//    extension(x: UserId) def value: String = UserId.

  type Username = NonEmptyFiniteString[30]
  object Username extends RefinedTypeOps[NonEmptyFiniteString[30], String] with CatsRefinedTypeOpsSyntax

  type EmailAddress = NonEmptyString
  object EmailAddress extends RefinedTypeOps[EmailAddress, String] with CatsRefinedTypeOpsSyntax

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
    def init: Either[String, EmailStatus] = Right(Unverified)

  case class Email(address: EmailAddress, status: EmailStatus)

  opaque type JoinDate = LocalDate
  object JoinDate:
    def apply(date: LocalDate): JoinDate = date
    def create (date: LocalDate): EitherNec[String, LocalDate] = date match
      case _ => Right(date)
  extension (x: JoinDate) def value: LocalDate = x

  case class User(id: UserId, name: Username, email: Email, joinDate: JoinDate)

  case class UserDto(name: String, email: String)

  object UserDto:
    import cats.syntax.EitherOps
    def toDomain(dto: UserDto): Either[NonEmptyChain[String], User] =
      val id = NanoIdUtils.randomNanoId()
      val iddd = UserId.from("")
      val emailAddress = EmailAddress.from(dto.email).leftMap(_ => "Email address must be in valid format.").toEitherNec
      val email = (emailAddress, EmailStatus.init.toEitherNec).parMapN(Email.apply)

      val hmm = Username.unsafeFrom("hello")
      (
        UserId.from(id).toEitherNec,
        Username.from(dto.name).leftMap(_ => "Username must be less than or equal to 50 chars.").toEitherNec,
        email,
        JoinDate.create(LocalDate.now())
        ).parMapN(User.apply)

  sealed trait UpdateUser
  case class UpdateNameAndEmail(name: Username, email: EmailAddress) extends UpdateUser
  case class UpdateName(name: Username) extends UpdateUser
  case class UpdateEmail(email: EmailAddress) extends UpdateUser

  object UpdateUser:
    import cats.syntax.functor._
    import io.circe.{ Decoder, Encoder }, io.circe.generic.auto._
    import io.circe.syntax._

    object GenericDerivation {
      implicit val encodeEvent: Encoder[UpdateUser] = Encoder.instance {
        case email @ UpdateEmail(_) => email.asJson
        case name @ UpdateName(_) => name.asJson
        case both @ UpdateNameAndEmail(_,_) => both.asJson
      }

      implicit val decodeEvent: Decoder[UpdateUser] =
        List[Decoder[UpdateUser]](
          Decoder[UpdateEmail].widen,
          Decoder[UpdateName].widen,
          Decoder[UpdateNameAndEmail].widen
        ).reduceLeft(_ or _)
  }
