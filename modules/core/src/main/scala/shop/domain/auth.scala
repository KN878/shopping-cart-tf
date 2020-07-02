package shop.domain

import java.util.UUID

import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.estatico.newtype.macros.newtype
import javax.crypto.Cipher

import scala.util.control.NoStackTrace

object auth {
  @newtype case class UserId(value: UUID)
  @newtype case class UserName(value: String)
  @newtype case class Password(value: String)

  @newtype case class EncryptedPassword(value: String)

  @newtype case class EncryptCipher(value: Cipher)
  @newtype case class DecryptCipher(value: Cipher)

  @newtype case class UserNameParam(value: NonEmptyString) {
    def toDomain: UserName = UserName(value.value.toLowerCase)
  }

  @newtype case class PasswordParam(value: NonEmptyString) {
    def toDomain: Password = Password(value.value)
  }

  case class CreateUser(
      username: UserNameParam,
      password: PasswordParam
  )

  case class UsernameInUse(userName: UserName) extends NoStackTrace
  case class InvalidUsernameOrPassword(userName: UserName) extends NoStackTrace
  case object UnsupportedOperation extends NoStackTrace

  case object TokenNotFound extends NoStackTrace

  case class LoginUser(
      username: UserNameParam,
      password: PasswordParam
  )

  @newtype case class ClaimContent(uuid: UUID)

  object ClaimContent {
    implicit val jsonDecoder: Decoder[ClaimContent] =
      Decoder.forProduct1("uuid")(ClaimContent.apply)
  }
}
