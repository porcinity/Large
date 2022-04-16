package io.porcinity.large.domain

import doobie.{Read, Write}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

import java.time.LocalDate
import doobie.implicits.legacy.localdate.*
import com.aventrix.jnanoid.jnanoid.*
import cats.data.*
import cats.implicits.*
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.cats.CatsRefinedTypeOpsSyntax
import eu.timepit.refined.types.numeric.{NonNegInt, PosDouble, PosFloat, PosInt, PosLong}
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined.*
import io.porcinity.large.common.{GetItem, GetItems}
import org.latestbit.circe.adt.codec.JsonTaggedAdt



object Article:
  implicit val articleCodec: Codec[Article] = deriveCodec
  implicit val getItemCodec: Codec[GetItem[Article]] = deriveCodec
  implicit val getItemsCodec: Codec[GetItems[Article]] = deriveCodec
  implicit val likeArticleCodec: Codec[LikeArticleDto] = deriveCodec

  final case class Article(
                      id: Id,
                      title: Title,
                      content: Content,
                      author: Author,
                      word_count: WordCount,
                      reading_time: ReadingTime,
                      likes: Likes,
                      visibility: Visibility,
                      published_on: ArticleDate,
                      last_updated: ArticleDate,
                      tags: Tags
                 )

  type Tags = List[String]
  object Tags:
    def from(list: List[String]): Either[NonEmptyChain[String], Tags] =
      Right(list)

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
    def fromString(input: String): Either[NonEmptyChain[String], Visibility] = input match
      case "Public" => Right(Public)
      case _ => Right(Private)
    def unsafeFromString(input: String): Visibility = input match
      case "Public" => Public
      case _ => Private
    def makeString(visibility: Visibility): String = visibility match
      case Public => "Public"
      case Private => "Private"

  type WordCount = PosInt
  object WordCount extends RefinedTypeOps[WordCount, Int] with CatsRefinedTypeOpsSyntax

  type ReadingTime = PosDouble
  object ReadingTime extends RefinedTypeOps[ReadingTime, Double] with CatsRefinedTypeOpsSyntax

  type Likes = NonNegInt
  object Likes extends RefinedTypeOps[Likes, Int] with CatsRefinedTypeOpsSyntax

  opaque type ArticleDate = LocalDate
  object ArticleDate:
    def apply(date: LocalDate): ArticleDate = date
    val create: Either[NonEmptyChain[String], ArticleDate] =
      Right(LocalDate.now())
    extension (x: ArticleDate) def value: LocalDate = x

  final case class ArticleDto(title: String, content: String, visibility: String)
  final case class LikeArticleDto(asUser: User.UserId)

  object ArticleDto:
    implicit val articleDtoCodec: Codec[ArticleDto] = deriveCodec
    def toDomain(dto: ArticleDto, author: User.UserId): Either[NonEmptyChain[String], Article] =
      val id = NanoIdUtils.randomNanoId()
      (
        Id.from(id).toEitherNec,
        Title.from(dto.title).toEitherNec,
        Content.from(dto.content).toEitherNec,
        Author.from(author.value).toEitherNec,
        WordCount.from(dto.content.split(" ").length).toEitherNec,
        ReadingTime.from(dto.content.split(" ").length / 200.0).toEitherNec,
        Likes.from(0).toEitherNec,
        Visibility.fromString(dto.visibility),
        ArticleDate.create,
        ArticleDate.create,
        Tags.from(List())
        ).parMapN(Article.apply)