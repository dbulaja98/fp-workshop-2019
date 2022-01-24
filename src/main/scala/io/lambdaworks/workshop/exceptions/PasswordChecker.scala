package io.lambdaworks.workshop.exceptions

object PasswordChecker {

  val minimumPasswordLength: Int = 5

  def validate(password: String): Either[List[Throwable], String] = {
    var left = List[Throwable]()
    if (minNumberOfChars(password, minimumPasswordLength).isLeft) left = left ::: List(InvalidLength)
    if (containsUpperCase(password).isLeft) left = left ::: List(MissingUppercase)
    if (containsLowerCase(password).isLeft) left = left ::: List(MissingLowercase)
    if (containsNumber(password).isLeft) left = left ::: List(MissingNumber)

    if (left.length == 0) Right(password) else Left(left)
  }

  private def minNumberOfChars(password: String, length: Int): Either[Throwable, String] =
    if (password.length >= length) Right(password) else Left(InvalidLength)

  private def containsUpperCase(password: String): Either[Throwable, String] =
    if (password.exists(ch => ch.isUpper)) Right(password) else Left(MissingUppercase)

  private def containsLowerCase(password: String): Either[Throwable, String] =
    if (password.exists(ch => ch.isLower)) Right(password) else Left(MissingLowercase)

  private def containsNumber(password: String): Either[Throwable, String] =
    if (password.exists(ch => ch.isDigit)) Right(password) else Left(MissingNumber)

  object InvalidLength    extends Throwable("Password must contain at least 5 characters.")
  object MissingUppercase extends Throwable("Password must contain uppercase letter.")
  object MissingLowercase extends Throwable("Password must contain lowercase letter.")
  object MissingNumber    extends Throwable("Password must contain number.")

}

