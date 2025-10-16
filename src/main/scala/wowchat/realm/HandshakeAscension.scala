package wowchat.realm

import org.bouncycastle.crypto.modes.ChaCha20Poly1305
import org.bouncycastle.crypto.params.{AEADParameters, KeyParameter}
import org.bouncycastle.math.ec.rfc7748.X25519

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.util.Random

// looks like they use https://github.com/jedisct1/libsodium

object HandshakeAscension {

  private val KEY_CONSTANT_1 = Array(
    0x36, 0x42, 0xAF, 0x85, 0x23, 0x69, 0x15, 0x4C, 0xFA, 0x11, 0x45, 0x95, 0x08, 0x80, 0x10, 0x82,
    0x80, 0xA4, 0x34, 0x1C, 0x26, 0xA3, 0x76, 0x43, 0x1B, 0x74, 0x1E, 0x2A, 0xAE, 0x9C, 0x29, 0x48
  ).map(_.toByte)

  private val KEY_CONSTANT_3 = Array(
    0x66, 0xD5, 0x2B, 0x01, 0xE0, 0x06, 0xCD, 0x24, 0x6F, 0x09, 0x00, 0x25, 0xD6, 0x31, 0x2C, 0x62,
    0xD1, 0x3E, 0x84, 0x7C, 0x98, 0x05, 0x95, 0x6A, 0x1C, 0x5A, 0x10, 0x36, 0x4B, 0xAA, 0x7D, 0x82
  ).map(_.toByte)

  // derived
  private val INPUT_CONSTANT_4 = Array(
    0xE8, 0x15, 0x73, 0x9F, 0x8E, 0xC8, 0x10, 0x72, 0x1B, 0x93, 0x55, 0x4C, 0xA2, 0xEA, 0xC5, 0x97,
    0xE0, 0x5F, 0x37, 0x52, 0x61, 0xDD, 0x72, 0xFF, 0x30, 0x83, 0x7D, 0xF9, 0x51, 0xC7, 0xA5, 0xED
  ).map(_.toByte)

  // session
  private val INPUT_CONSTANT_5 = Array(
    0x26, 0x98, 0x6C, 0x8A, 0x73, 0xD2, 0x4B, 0xC4, 0x1C, 0xF3, 0x86, 0xBC, 0xB5, 0x84, 0x92, 0x41,
    0x6F, 0xB5, 0x79, 0x78, 0x4E, 0x19, 0x57, 0x70, 0x1A, 0x88, 0x9D, 0x97, 0xB6, 0x55, 0x01, 0x40
  ).map(_.toByte)

  private val INPUT_CONSTANT_6 = Array(
    0x4F, 0x4B
  ).map(_.toByte)

  private def generate_key(): Array[Byte] = {
    val result = Array.fill[Byte](32)(0)
    scala.util.Random.nextBytes(result)
    X25519.clampPrivateKey(result)
    result
  }

  private def calculate_key_public(key_private: Array[Byte]): Array[Byte] = {
    val result = Array.fill[Byte](32)(0)
    X25519.scalarMultBase(key_private, 0, result, 0)
    result
  }

  private def calculate_key_shared(key_1: Array[Byte]): Array[Byte] = {
    val result = Array.fill[Byte](32)(0)
    X25519.scalarMult(key_1, 0, KEY_CONSTANT_1, 0, result, 0)
    result
  }
}

class HandshakeAscension {

  private val KEY_CONSTANT_2 = Array(
    0x33, 0xBA, 0x31, 0x28, 0xEE, 0x61, 0x4B, 0x58, 0x45, 0xE0, 0x6B, 0x0D, 0xAD, 0x17, 0x6A, 0x9C,
    0x79, 0x34, 0x4D, 0xD7, 0xA7, 0xA1, 0xE2, 0xE8, 0xD8, 0xAD, 0x09, 0x7D, 0xA9, 0xB5, 0x7F, 0x01
  ).map(_.toByte)

  private val NONCE_CONSTANT_2 = Array(
    0x92, 0x01, 0x00, 0x8E, 0xCA, 0xFA, 0x7D, 0x60, 0xE0, 0xAC, 0xC8, 0x1E
  ).map(_.toByte)



  private val random_key_secret = HandshakeAscension.generate_key()
  val key_public = HandshakeAscension.calculate_key_public(random_key_secret)
  private val key_shared = HandshakeAscension.calculate_key_shared(random_key_secret)

  val random_nonce_1 = {
    val result = Array.fill[Byte](12)(0)
    Random.nextBytes(result)
    result
  }

  val key_derived = derive_key(HandshakeAscension.KEY_CONSTANT_3, key_shared, HandshakeAscension.INPUT_CONSTANT_4, 32)
  val key_session = derive_key(HandshakeAscension.KEY_CONSTANT_3, key_shared, HandshakeAscension.INPUT_CONSTANT_5, 40)

  val proof_2 = calculate_proof(key_derived, HandshakeAscension.INPUT_CONSTANT_6)


  def encrypt_password(password: Array[Byte]) : (Array[Byte], Array[Byte]) = {
    val encrypter = new ChaCha20Poly1305
    encrypter.init(
      true,
      new AEADParameters(
        new KeyParameter(key_derived),
        16 * 8,
        random_nonce_1
      )
    )
    val result = Array.fill[Byte](password.length + 16)(0)
    val processed = encrypter.processBytes(password, 0, password.length, result, 0)
    encrypter.doFinal(result, processed)
    (result.take(password.length), result.drop(password.length))
  }

  def encrypt_packet(data: Array[Byte], header: Array[Byte]) = {
    val encrypter = new ChaCha20Poly1305
    encrypter.init(
      true,
      new AEADParameters(
        new KeyParameter(KEY_CONSTANT_2),
        16 * 8,
        NONCE_CONSTANT_2,
        header
      )
    )
    val result = Array.fill[Byte](data.length + 16)(0)
    //encrypter.processAADBytes(header, 0, header.length)
    val processed = encrypter.processBytes(data, 0, data.length, result, 0)
    encrypter.doFinal(result, processed)
    (result.take(data.length), result.drop(data.length))
  }  

  private def derive_key(key: Array[Byte], input_1: Array[Byte], input_2: Array[Byte], size: Int): Array[Byte] = {
    val algorithm = "HmacSHA256"
    val hasher_1 = Mac.getInstance(algorithm)
    hasher_1.init(new SecretKeySpec(key, algorithm))
    val interim = hasher_1.doFinal(input_1)
    val hasher_2 = Mac.getInstance(algorithm)
    hasher_2.init(new SecretKeySpec(interim, algorithm))
    hasher_2.update(input_2)
    val result = hasher_2.doFinal(Array[Byte](1))
    if (size == 32) {
      return result
    }
    {
      val hasher_2 = Mac.getInstance(algorithm)
      hasher_2.init(new SecretKeySpec(interim, algorithm))
      hasher_2.update(result)
      hasher_2.update(input_2)
      val result_ = hasher_2.doFinal(Array[Byte](2))
      (result ++ result_).take(size)
    }
  }

  private def calculate_proof(key: Array[Byte], input_1: Array[Byte]): Array[Byte] = {
    val algorithm = "HmacSHA256"
    val hasher = Mac.getInstance(algorithm)
    hasher.init(new SecretKeySpec(key, algorithm))
    hasher.doFinal(input_1)
  }

}
