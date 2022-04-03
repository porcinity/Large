package models

import doobie.{Read, Write}
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}
import io.circe.syntax.*
import io.circe.generic.auto.*

import java.time.LocalDate
import com.aventrix.jnanoid.jnanoid.*
import eu.timepit.refined.boolean.And
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.numeric.*

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
    implicit val stringCodec: Codec[GetItems[String]] = deriveCodec
    implicit val GetItemCodec: Codec[GetItem[User]] = deriveCodec
    implicit val GetItemsCodec: Codec[GetItems[User]] = deriveCodec
    implicit val dtoCodec: Codec[UserDto] = deriveCodec
    implicit val userCodec: Codec[User] = deriveCodec

  case class UserDto(name: String, bio: String, email: String)
  case class User(
                   id: UserId,
                   name: Username,
                   bio: Biography,
                   email: Email,
                   tier: MembershipTier,
                   followers: Followers,
                   following: Following,
                   likedArticles: Liked,
                   joinDate: JoinDate,
                   articles: Articles
                 )

  type Articles = List[String]
  object Articles:
    def from(list: List[String]): Either[NonEmptyChain[String], Articles] =
      Right(list)
      
  type UserId = NonEmptyFiniteString[30]
  object UserId extends RefinedTypeOps[UserId, String] with CatsRefinedTypeOpsSyntax

  type Username = NonEmptyFiniteString[15]
  object Username extends RefinedTypeOps[Username, String] with CatsRefinedTypeOpsSyntax:
    def validate(input: String): Either[NonEmptyChain[String], Username] =
      Username.from(input).leftMap(_ => "Username must be less than or equal to 15 chars.").toEitherNec

  type ValidEmail = Refined[String, MatchesRegex["""^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"""]]

  type EmailAddress = ValidEmail
  object EmailAddress extends RefinedTypeOps[ValidEmail, String] with CatsRefinedTypeOpsSyntax:
    def validate(input: String): Either[NonEmptyChain[String], EmailAddress] =
      EmailAddress.from(input).leftMap(_ => "Email must be in valid format.").toEitherNec

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

  enum MembershipTier derives JsonTaggedAdt.PureEncoder, JsonTaggedAdt.PureDecoder:
    case Free
    case Trial
    case Premium

  object MembershipTier:
    val init: Either[NonEmptyChain[String], MembershipTier] = Right(Free)
    def fromString(input: String): MembershipTier = input match
      case "Trial" => Trial
      case "Premium" => Premium
      case _ => Free
    def makeString(tier: MembershipTier): String = tier match
      case Trial => "Trial"
      case Free => "Free"
      case Premium => "Premium"

  type Liked = NonNegInt
  object Liked extends RefinedTypeOps[Liked, Int] with CatsRefinedTypeOpsSyntax

  type Followers = NonNegInt
  object Followers extends RefinedTypeOps[Followers, Int] with CatsRefinedTypeOpsSyntax

  type Following = NonNegInt
  object Following extends RefinedTypeOps[Following, Int] with CatsRefinedTypeOpsSyntax

  type Biography = NonEmptyString
  object Biography extends RefinedTypeOps[Biography, String] with CatsRefinedTypeOpsSyntax

  object UserDto:
    import cats.syntax.EitherOps
    def toDomain(dto: UserDto): Either[NonEmptyChain[String], User] =
      val id = NanoIdUtils.randomNanoId()
      val emailAddress = EmailAddress.validate(dto.email)
      val email = (emailAddress, EmailStatus.init.toEitherNec).parMapN(Email.apply)

      (
        UserId.from(id).toEitherNec,
        Username.validate(dto.name),
        Biography.from(dto.bio).leftMap(_ => "Bio cannot be empty.").toEitherNec,
        email,
        MembershipTier.init,
        Followers.from(0).toEitherNec,
        Following.from(0).toEitherNec,
        Liked.from(0).toEitherNec,
        JoinDate.create(LocalDate.now()),
        Articles.from(List())
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
