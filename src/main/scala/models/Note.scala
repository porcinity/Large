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
      case LessOrEqual150() => value.validNec
      case _ => "Note's title must be less than or equal to 150 chars.".invalidNec
    extension (x: NoteTitle)
      @targetName("value_NoteTitle")
      def titleVal: String = x

  opaque type NoteContent = String

  object NoteContent:
    def apply(value: String): NoteContent = value
    def create(value: String): ValidationResult[NoteContent] = value match
      case LessOrEqual15k() => value.validNec
      case _ => "Note's content must be less than or equal to 15,000 chars.".invalidNec
    extension (x: NoteContent) def valooo: String = x

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