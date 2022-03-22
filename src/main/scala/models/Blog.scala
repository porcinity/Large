package models

import doobie.{Read, Write}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

import java.time.LocalDate
import doobie.implicits.legacy.localdate.*
import com.aventrix.jnanoid.jnanoid.*

import cats.data.*
import cats.data.Validated.*
import cats.implicits.*
import models.ValidationExtractors.*

object Blog:
  type ValidationError = String
  type ValidationResult[A] = ValidatedNec[ValidationError, A]

  case class Blog(id: BlogId, title: BlogTitle, content: BlogContent, author: BlogAuthor)

  opaque type BlogId = String

  object BlogId:
    def apply(value: String): BlogId = value
    def create(value: String): ValidationResult[BlogId] = value.validNec

  extension (x: BlogId)
    def value: String = x

  opaque type BlogTitle = String

  object BlogTitle:
    def apply(value: String): BlogTitle = value
    def create(value: String): ValidationResult[BlogTitle] = value match
      case EmptyName() => "Blog title cannot be empty.".invalidNec
      case FewerThan5() => "Blog title must be longer than 5 characters.".invalidNec
      case Over150() => "Blog title cannot be longer than 150 characters.".invalidNec
      case _ => value.validNec

  extension (x: BlogTitle)
    def titleVal: String = x

  opaque type BlogContent = String

  object BlogContent:
    def apply(value: String): BlogContent = value
    def create(value: String): ValidationResult[BlogContent] = value match
      case EmptyName() => "Blog cannot be empty.".invalidNec
      case FewerThan5() => "Blog must be longer than 5 characters.".invalidNec
      case Over15k() => "Blog must be less than 15,000 characters.".invalidNec
      case _ => value.validNec
  extension (x: BlogContent) def v: String = x

  opaque type BlogAuthor = String

  object BlogAuthor:
    def apply(value: String): BlogAuthor = value
    def create(value: String): ValidationResult[BlogAuthor] = value.validNec
    extension (x: BlogAuthor) def authorVal: String = x


  implicit val blogCodec: Codec[Blog] = deriveCodec[Blog]
  implicit val blogRead: Read[Blog] =
    Read[(String, String, String, String)].map { case (id, title, content, authorId) =>
      Blog(BlogId(id), BlogTitle(title), BlogContent(content), BlogAuthor(authorId))
    }
  implicit val blogWrite: Write[Blog] =
    Write[(String, String, String, String)].contramap { blog =>
      (blog.id.value, blog.title.value, blog.content.v, blog.author.value)
    }

  case class BlogDto(title: String, content: String, authorId: String)

  object BlogDto:
    implicit val blogDtoCodec: Codec[BlogDto] = deriveCodec[BlogDto]
    def toDomain(dto: BlogDto): ValidationResult[Blog] =
      val id = NanoIdUtils.randomNanoId()
      (BlogId.create(id),
        BlogTitle.create(dto.title),
        BlogContent.create(dto.content),
        BlogAuthor.create(dto.authorId)).mapN(Blog.apply)