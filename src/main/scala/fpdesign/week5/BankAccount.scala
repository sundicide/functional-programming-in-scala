package fpdesign.week5

class BankAccount {
  private var balance = Var(0)
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance() = balance() + amount
  }

  def withdraw(amount: Int): Int =
    if (0 < amount && amount <= balance()) {
      balance() = balance() - amount
    } else throw new Error("insufficient funds")
}

object BankAccount {
  def main(args: Array[String]): Unit = {
    val acct = new BankAccount
    acct deposit 50
    acct withdraw 20
    acct withdraw 20
    acct withdraw 20
  }
}
