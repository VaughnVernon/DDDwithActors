import akka.persistence.PersistentActor

case class OpenAccount(accountNumber: String, initialBalance: Int)
case class DepositFunds(amount: Int)
case class WithdrawFunds(amount: Int)

case class AccountOpened(accountNumber: String, initialBalance: Int)
case class FundsDeposited(accountNumber: String, amount: Int)
case class FundsWithdrawn(accountNumber: String, amount: Int)

class Account extends PersistentActor {
  var state: Option[AccountState] = None

  override def persistenceId: String = self.path.name

  override def receiveCommand: Receive = {
  	case open: OpenAccount =>
  	  persist(AccountOpened(open.accountNumber, open.initialBalance)) { event =>
  	  	updateWith(event)
  	  	sender ! (state, event)
  	  }
  	case deposit: DepositFunds =>
  	  persist(FundsDeposited(accountNumber, deposit.amount)) { event =>
  	  	updateWith(event)
  	  	sender ! (state, event)
  	  }
  	case withdraw: WithdrawFunds =>
      persist(FundsWithdrawn(accountNumber, withdraw.amount)) { event =>
      	updateWith(event)
      	sender ! (state, event)
      }
  }

  override def receiveRecover: Receive = {
  	case event: AccountOpened => updateWith(event)
  	case event: FundsDeposited => updateWith(event)
  	case event: FundsWithdrawn => updateWith(event)
  }

  def accountNumber: String =
    state.getOrElse(AccountState("unknown", 0)).accountNumber

  def updateWith(event: AccountOpened): Unit =
    state = Option(AccountState(event.accountNumber, event.initialBalance))

  def updateWith(event: FundsDeposited): Unit =
    state = state.map(_.fromDeposited(event.amount))

  def updateWith(event: FundsWithdrawn): Unit =
    state = state.map(_.fromWithdrawn(event.amount))
}

case class AccountState(accountNumber: String, balance: Int) {
  def fromDeposited(amount: Int): AccountState =
    AccountState(accountNumber, balance + amount)

  def fromWithdrawn(amount: Int): AccountState =
  	AccountState(accountNumber, balance - amount)
}
