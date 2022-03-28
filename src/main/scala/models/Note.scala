package models

import doobie.{Read, Write}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

import java.time.LocalDate
import doobie.implicits.legacy.localdate.*
import com.aventrix.jnanoid.jnanoid.*

import cats.data.*
import cats.implicits.*
import models.ValidationExtractors.*
import scala.annotation.targetName

object Note:
  type ValidationError = String
  type ValidationResult[A] = ValidatedNec[ValidationError, A]

  case class Note(id: NoteId, title: NoteTitle, content: NoteContent, author: NoteAuthor)

  opaque type NoteId = String

  object NoteId:
    def apply(value: String): NoteId = value
    def create(value: String): ValidationResult[NoteId] = value.validNec

  extension (x: NoteId)
    @targetName("value_NoteId")
    def value: String = x

  opaque type NoteTitle = String

  object NoteTitle:
    def apply(value: String): NoteTitle = value
    def create(value: String): ValidationResult[NoteTitle] = value match
      case EmptyName() => "Note title cannot be empty.".invalidNec
      case FewerThan5() => "Note title must be longer than 5 characters.".invalidNec
      case Over150() => "Note title cannot be longer than 150 characters.".invalidNec
      case _ => value.validNec
    def createEither(v: String): Either[String, NoteTitle] = v match {
      case EmptyName() => Left("No empty is bad")
      case _ => Right(v)
    }

  extension (x: NoteTitle)
    @targetName("value_NoteTitle")
    def titleVal: String = x

  opaque type NoteContent = String

  object NoteContent:
    def apply(value: String): NoteContent = value
    def create(value: String): ValidationResult[NoteContent] = value match
      case EmptyName() => "Note cannot be empty.".invalidNec
      case FewerThan5() => "Note must be longer than 5 characters.".invalidNec
      case Over15k() => "Note must be less than 15,000 characters.".invalidNec
      case _ => value.validNec
    def createEither(v: String): Either[String, NoteContent] = v match {
      case EmptyName() => Left("Content must b here")
      case _ => Right(v)
    }
  extension (x: NoteContent) def v: String = x

  opaque type NoteAuthor = String

  object NoteAuthor:
    def apply(value: String): NoteAuthor = value
    def create(value: String): ValidationResult[NoteAuthor] = value.validNec
  extension (x: NoteAuthor)
    @targetName("value_NoteAuthor")
    def value: String = x
  
  implicit val noteCodec: Codec[Note] = deriveCodec[Note]

  case class NoteDto(title: String, content: String, author: String)

  object NoteDto:
    implicit val noteDtoCodec: Codec[NoteDto] = deriveCodec[NoteDto]
    def toDomain(dto: NoteDto): ValidationResult[Note] =
      val id = NanoIdUtils.randomNanoId()
      (NoteId.create(id),
        NoteTitle.create(dto.title),
        NoteContent.create(dto.content),
        NoteAuthor.create(dto.author)).mapN(Note.apply)