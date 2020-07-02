package shop.algebras

import cats.effect.Sync
import cats.implicits._
import javax.crypto.{ Cipher, SecretKeyFactory }
import javax.crypto.spec.{ PBEKeySpec, SecretKeySpec }
import shop.config.data.PasswordSalt
import shop.domain.auth.{ DecryptCipher, EncryptCipher, EncryptedPassword, Password }

trait Crypto {
  def encrypt(password: Password): EncryptedPassword
  def decrypt(password: EncryptedPassword): Password
}

final class LiveCrypto(
    eCipher: EncryptCipher,
    dCipher: DecryptCipher
) extends Crypto {
  private val KEY = "=SomeKeyLol="

  override def encrypt(password: Password): EncryptedPassword = {
    val bytes      = password.value.getBytes("UTF-8")
    val res        = new String(eCipher.value.doFinal(bytes), "UTF-8")
    val removeNull = res.replaceAll("\\u0000", KEY)
    EncryptedPassword(removeNull)
  }

  override def decrypt(password: EncryptedPassword): Password = {
    val bytes      = password.value.getBytes("UTF-8")
    val res        = new String(dCipher.value.doFinal(bytes), "UTF-8")
    val insertNull = res.replaceAll(KEY, "\\u0000")
    Password(insertNull)
  }
}

object LiveCrypto {
  def make[F[_]: Sync](secret: PasswordSalt): F[Crypto] =
    Sync[F]
      .delay {
        val salt     = secret.value.value.value.getBytes("UTF-8")
        val keySpec  = new PBEKeySpec("password".toCharArray(), salt, 65536, 256)
        val factory  = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
        val bytes    = factory.generateSecret(keySpec).getEncoded
        val sKeySpec = new SecretKeySpec(bytes, "AES")
        val eCipher  = EncryptCipher(Cipher.getInstance("AES"))
        eCipher.value.init(Cipher.ENCRYPT_MODE, sKeySpec)
        val dCipher = DecryptCipher(Cipher.getInstance("AES"))
        dCipher.value.init(Cipher.DECRYPT_MODE, sKeySpec)
        (eCipher, dCipher)
      }
      .map {
        case (e, d) => new LiveCrypto(e, d)
      }
}
