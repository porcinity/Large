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
import common.{GetItem, GetItems}
object User:

  object Codecs:
    implicit val GetItemCodec: Codec[GetItem[User]] = deriveCodec
    implicit val GetItemsCodec: Codec[GetItems[User]] = deriveCodec
    implicit val dtoCodec: Codec[UserDto] = deriveCodec[UserDto]
    implicit val userCodec: Codec[User] = deriveCodec[User]

  case class UserDto(name: String, email: String)
  case class User(id: UserId, name: Username, email: Email, joinDate: JoinDate)

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
  object Username extends RefinedTypeOps[NonEmptyFiniteString[30], String] with CatsRefinedTypeOpsSyntax:
    def validate(input: String) =
      Username.from(input).leftMap(_ => "Username must be less than or equal to 30 chars.").toEitherNec

  type EmailAddress = NonEmptyString
  object EmailAddress extends RefinedTypeOps[EmailAddress, String] with CatsRefinedTypeOpsSyntax:
    def validate(input: String) = EmailAddress.from(input).leftMap(_ => "Email must be valid lmao").toEitherNec

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
    def init: Either[String, EmailStatus] = Right(Unverified)

  case class Email(address: EmailAddress, status: EmailStatus)

  opaque type JoinDate = LocalDate

  object JoinDate:
    def apply(date: LocalDate): JoinDate = date
    def create (date: LocalDate): EitherNec[String, LocalDate] = date match
      case _ => Right(date)
  extension (x: JoinDate) def value: LocalDate = x

  object UserDto:
    import cats.syntax.EitherOps
    def toDomain(dto: UserDto): Either[NonEmptyChain[String], User] =
      val id = NanoIdUtils.randomNanoId()
      val emailAddress = EmailAddress.validate(dto.email)
      val email = (emailAddress, EmailStatus.init.toEitherNec).parMapN(Email.apply)

      (
        UserId.from(id).toEitherNec,
        Username.validate(dto.name),
        email,
        JoinDate.create(LocalDate.now())
        ).parMapN(User.apply)

  enum UpdateUser:
    case Name(name: String)
    case Email(email: String)
    case UpdateNameAndEmail(name: String, email: String)


  object UpdateUser:
    import cats.syntax.functor._
    import io.circe.{ Decoder, Encoder }, io.circe.generic.auto._
    import io.circe.syntax._
    import monocle.Prism
    import monocle.macros.GenPrism
    import monocle.syntax.all.*
    import monocle.refined.all.*

    def of(dto: UpdateUser, user: User): Either[NonEmptyChain[String], User] = dto match
      case UpdateUser.UpdateNameAndEmail(name, email) =>
        (
          EmailAddress.validate(email),
          Username.validate(name)
          )
          .parMapN( (e, n) =>
          user.focus(_.name).replace(n).focus(_.email.address).replace(e))
      case UpdateUser.Name(n) =>
        Username.validate(n).map(user.focus(_.name).replace)
      case UpdateUser.Email(e) =>
        EmailAddress.validate(e).map(user.focus(_.email.address).replace)

//    val rawName: Prism[UpdateUser, UpdateName] = GenPrism[UpdateUser, UpdateName]

    object GenericDerivation {
      implicit val encodeEvent: Encoder[UpdateUser] = Encoder.instance {
        case both @ UpdateNameAndEmail(_,_) => both.asJson
        case email @ Email(_) => email.asJson
        case name @ Name(_) => name.asJson
      }

      implicit val decodeEvent: Decoder[UpdateUser] =
        List[Decoder[UpdateUser]](
          Decoder[UpdateNameAndEmail].widen,
          Decoder[Email].widen,
          Decoder[Name].widen
        ).reduceLeft(_ or _)
  }
