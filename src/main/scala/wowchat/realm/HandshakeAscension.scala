package wowchat.realm

// looks like they use https://github.com/jedisct1/libsodium

class HandshakeAscension {

  val key_constant_1 = java.util.HexFormat.of.parseHex("3642af852369154cfa1145950880108280a4341c26a376431b741e2aae9c2948")
  val key_constant_2 = java.util.HexFormat.of.parseHex("b6eb6241b5829b689f07c91f5731ba1bbb671c3fa2f7001c2249e3fa52dd8702")
  val key_constant_3 = java.util.HexFormat.of.parseHex("4f4b")

  var random_key_secret = generate_key()
  var key_public = calculate_key_public(random_key_secret)
  var key_shared = calculate_key_shared(random_key_secret, key_constant_1)

  var random_nonce = (() => { val result = Array.fill[Byte](12)(0); scala.util.Random.nextBytes(result); result })()

  var key_derived = derive_key(key_constant_2, key_shared, "derived".getBytes("utf-8"), 32)
  var key_session = derive_key(key_constant_2, key_shared, "session".getBytes("utf-8"), 40)

  var proof_2 = calculate_proof(key_derived, key_constant_3)

  private def generate_key(): Array[Byte] = {
    val result = Array.fill[Byte](32)(0)
    scala.util.Random.nextBytes(result)
    org.bouncycastle.math.ec.rfc7748.X25519.clampPrivateKey(result)
    result
  }

  private def calculate_key_public(key_private: Array[Byte]): Array[Byte] = {
    val result = Array.fill[Byte](32)(0)
    org.bouncycastle.math.ec.rfc7748.X25519.scalarMultBase(key_private, 0, result, 0)
    result
  }

  private def calculate_key_shared(key_1: Array[Byte], key_2: Array[Byte]): Array[Byte] = {
    val result = Array.fill[Byte](32)(0)
    org.bouncycastle.math.ec.rfc7748.X25519.scalarMult(key_1, 0, key_2, 0, result, 0)
    result
  }

  private def derive_key(key: Array[Byte], input_1: Array[Byte], input_2: Array[Byte], size: Int): Array[Byte] = {
    val algorithm = "HmacSHA256"
    val hasher_1 = javax.crypto.Mac.getInstance(algorithm)
    hasher_1.init(new javax.crypto.spec.SecretKeySpec(key, algorithm))
    val interim = hasher_1.doFinal(input_1)
    val hasher_2 = javax.crypto.Mac.getInstance(algorithm)
    hasher_2.init(new javax.crypto.spec.SecretKeySpec(interim, algorithm))
    hasher_2.update(input_2)
    val result = hasher_2.doFinal(Array[Byte](1))
    if (size == 32) {
      return result
    }
    {
      val hasher_2 = javax.crypto.Mac.getInstance(algorithm)
      hasher_2.init(new javax.crypto.spec.SecretKeySpec(interim, algorithm))
      hasher_2.update(result)
      hasher_2.update(input_2)
      val result_ = hasher_2.doFinal(Array[Byte](2))
      (result ++ result_).take(size)
    }
  }

  private def calculate_proof(key: Array[Byte], input_1: Array[Byte]): Array[Byte] = {
    val algorithm = "HmacSHA256"
    val hasher = javax.crypto.Mac.getInstance(algorithm)
    hasher.init(new javax.crypto.spec.SecretKeySpec(key, algorithm))
    hasher.doFinal(input_1)
  }

  def encrypt_password(password: Array[Byte]) : (Array[Byte], Array[Byte]) = {
    val encrypter = new org.bouncycastle.crypto.modes.ChaCha20Poly1305
    encrypter.init(
      true,
      new org.bouncycastle.crypto.params.AEADParameters(
        new org.bouncycastle.crypto.params.KeyParameter(key_derived),
        16 * 8,
        random_nonce
      )
    )
    val result = Array.fill[Byte](password.length + 16)(0)
    encrypter.processBytes(password, 0, password.length, result, 0)
    encrypter.doFinal(result, 0)
    (result.take(password.length), result.drop(password.length))
  }
}
