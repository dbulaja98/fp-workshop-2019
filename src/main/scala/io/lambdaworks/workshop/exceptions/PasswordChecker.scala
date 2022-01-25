package io.lambdaworks.workshop.exceptions

object PasswordChecker {

  val minimumPasswordLength: Int = 5

  def validate(password: String): Either[List[Throwable], String] = {
    val errors = List(
      minNumberOfChars(password, minimumPasswordLength), containsUpperCase(password),
      containsLowerCase(password), containsNumber(password)
    ).collect{
      case Left(error) => error
    }

    if (errors.isEmpty) Right(password) else Left(errors)
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

