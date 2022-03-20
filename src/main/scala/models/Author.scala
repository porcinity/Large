package models

import doobie.{Read, Write}
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}
import io.circe.syntax.*
import io.circe.generic.auto.*

import java.time.LocalDate
import doobie.implicits.legacy.localdate._
import com.aventrix.jnanoid.jnanoid.*

object Author:
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
        author.id.value,
        author.name.value,
        author.email.address.value, 
        EmailStatus.makeString(author.email.status),
        author.joinDate
      )
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

  opaque type AuthorId = String

  object AuthorId:
    def apply(value: String): AuthorId = value
  extension (x: AuthorId) def value: String = x

  opaque type EmailAddress = String
  object EmailAddress:
    def apply(value: String): EmailAddress = value
  extension (x: EmailAddress) def v: String = x

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

  opaque type UserJoinDate = LocalDate
//  object JoinDate:
//    def apply(date: UserJoinDate): UserJoinDate = date
  extension (x: UserJoinDate) def value: LocalDate = x

  case class Author(id: AuthorId, name: Name, email: Email, joinDate: UserJoinDate)

  case class AuthorDto(name: String, email: String)
  
  object AuthorDto:
    def toDomain(dto: AuthorDto): Author =
      val id = NanoIdUtils.randomNanoId()
      Author(
        AuthorId(id),
        Name(dto.name),
        Email(
          EmailAddress(dto.email),
          EmailStatus.Unverified
        ),
        LocalDate.now()
      )