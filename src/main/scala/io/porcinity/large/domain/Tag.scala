package io.porcinity.large.domain

import cats.data.NonEmptyChain
import Article.Id
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import com.aventrix.jnanoid.jnanoid.*
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.cats.CatsRefinedTypeOpsSyntax
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined.*
import cats.syntax.all.*
import io.porcinity.large.common.{GetItem, GetItems}

object Tag {
  implicit val tagCodec: Codec[Tag] = deriveCodec
  implicit val getItemCodec: Codec[GetItem[TagName]] = deriveCodec
  implicit val getItemsCodec: Codec[GetItems[TagName]] = deriveCodec

  final case class Tag(id: TagId, name: TagName, articleId: TaggedArticle)

  type TagId = NonEmptyString
  object TagId extends RefinedTypeOps[TagId, String] with CatsRefinedTypeOpsSyntax

  type TagName = NonEmptyString
  object TagName extends RefinedTypeOps[TagName, String] with CatsRefinedTypeOpsSyntax

  type TaggedArticle = NonEmptyString
  object TaggedArticle extends RefinedTypeOps[TaggedArticle, String] with CatsRefinedTypeOpsSyntax

  final case class TagDto(name: String)

  object TagDto:
    implicit val tagDtoCodec: Codec[TagDto] = deriveCodec[TagDto]

    def toDomain(dto: TagDto, id: Article.Id): Either[NonEmptyChain[String], Tag] =
      val tagId = NanoIdUtils.randomNanoId()
      (
        TagId.from(tagId).toEitherNec,
        TagName.from(dto.name).toEitherNec,
        TaggedArticle.from(id.value).toEitherNec
      ).parMapN(Tag.apply)
}