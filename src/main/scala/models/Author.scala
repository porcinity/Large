package models

import doobie.{Read, Write}
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}
import io.circe.syntax.*
import io.circe.generic.auto.*

import java.time.LocalDate
import doobie.implicits.legacy.localdate.*
import com.aventrix.jnanoid.jnanoid.*

import scala.annotation.targetName

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

  opaque type Name = String

  object Name:
    def apply(value: String): Name = value
    def create(value: String): Either[String, Name] = value match {
      case "" => Left("Empty")
      case x if x.length > 4 => Left("too long tho")
      case _ => Right(value)
    }
    extension (x: Name)
      def value: String = x

  opaque type Age = Int

  object Age:
    def apply(value: Int): Age = value

    extension (x: Age) def value: Int = x

  opaque type Weight = Int

  object Weight:
    def apply(value: Int): Weight = value

  opaque type AuthorId = String

  object AuthorId:
    def apply(value: String): AuthorId = value
    def value(authorId: AuthorId): String = authorId
  extension (x: AuthorId) {
    @targetName("value_AuthorId")
    def value: String = x
  }

  opaque type EmailAddress = String
  object EmailAddress:
    def apply(value: String): EmailAddress = value
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

  case class Email(address: EmailAddress, status: EmailStatus)

  opaque type JoinDate = LocalDate
  object JoinDate:
    def apply(date: JoinDate): JoinDate = date
  extension (x: JoinDate) def value: LocalDate = x

  case class Author(id: AuthorId, name: Name, email: Email, joinDate: JoinDate)

  case class AuthorDto(name: String, email: String)
  
  object AuthorDto:
    def toDomain(dto: AuthorDto): Either[String, Author] =
      val id = NanoIdUtils.randomNanoId()
      val name = Name.create(dto.name)
      for {
        n <- name
      } yield Author(
        AuthorId(id),
        n,
        Email(
          EmailAddress(dto.email),
          EmailStatus.Unverified
        ),
        JoinDate(LocalDate.now())
      )