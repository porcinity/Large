package models

import doobie.{Read, Write}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

import java.time.LocalDate
import doobie.implicits.legacy.localdate.*
import com.aventrix.jnanoid.jnanoid.*
import cats.data.*
import cats.implicits.*
import common.{GetItem, GetItems}
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.cats.CatsRefinedTypeOpsSyntax
import eu.timepit.refined.types.numeric.{NonNegInt, PosDouble, PosFloat, PosInt, PosLong}
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined.*
import org.latestbit.circe.adt.codec.JsonTaggedAdt



object Note:
  implicit val noteCodec: Codec[Note] = deriveCodec[Note]
  implicit val GetItemCodec: Codec[GetItem[Note]] = deriveCodec
  implicit val GetItemsCodec: Codec[GetItems[Note]] = deriveCodec

  case class Note(
                   id: Id,
                   title: Title,
                   content: Content,
                   author: Author,
                   word_count: WordCount,
                   reading_time: ReadingTime,
                   likes: Likes,
                   visibility: Visibility,
                   published_on: BlogDate,
                   last_updated: BlogDate,
                 )

  type Id = NonEmptyString
  object Id extends RefinedTypeOps[NonEmptyString, String] with CatsRefinedTypeOpsSyntax

  type Title = NonEmptyString
  object Title extends RefinedTypeOps[NonEmptyString, String] with CatsRefinedTypeOpsSyntax

  type Content = NonEmptyString
  object Content extends RefinedTypeOps[NonEmptyString, String] with CatsRefinedTypeOpsSyntax

  type Author = NonEmptyString
  object Author extends RefinedTypeOps[NonEmptyString, String] with CatsRefinedTypeOpsSyntax

  enum Visibility derives JsonTaggedAdt.PureEncoder, JsonTaggedAdt.PureDecoder:
    case Public
    case Private

  object Visibility:
    val init: Either[NonEmptyChain[String], Visibility] = Right(Private)
    def fromString(input: String): Visibility = input match
      case "Public" => Public
      case _ => Private

  type WordCount = PosInt
  object WordCount extends RefinedTypeOps[WordCount, Int] with CatsRefinedTypeOpsSyntax

  type ReadingTime = PosDouble
  object ReadingTime extends RefinedTypeOps[ReadingTime, Double] with CatsRefinedTypeOpsSyntax

  type Likes = NonNegInt
  object Likes extends RefinedTypeOps[Likes, Int] with CatsRefinedTypeOpsSyntax

  opaque type BlogDate = LocalDate
  object BlogDate:
    def apply(date: LocalDate): BlogDate = date
    val create: Either[NonEmptyChain[String], BlogDate] =
      Right(LocalDate.now())
    extension (x: BlogDate) def value: LocalDate = x

  case class NoteDto(title: String, content: String, author: String, visibility: String)

  object NoteDto:
    implicit val noteDtoCodec: Codec[NoteDto] = deriveCodec[NoteDto]
    def toDomain(dto: NoteDto): Either[NonEmptyChain[String], Note] =
      val id = NanoIdUtils.randomNanoId()
      (Id.from(id).toEitherNec,
        Title.from(dto.title).toEitherNec,
        Content.from(dto.content).toEitherNec,
        Author.from(dto.author).toEitherNec,
        WordCount.from(dto.content.length).toEitherNec,
        ReadingTime.from(dto.content.length / 200.0).toEitherNec,
        Likes.from(0).toEitherNec,
        Visibility.init,
        BlogDate.create,
        BlogDate.create
        ).parMapN(Note.apply)