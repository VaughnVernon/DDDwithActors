import akka.actor._

import com.typesafe.config._

object AccountRunner extends App {
  val system = ActorSystem("accountsContext")
  val accountService = system.actorOf(Props[AccountService], "accountService")
  accountService ! ManageAccount("A-1234")

  Thread.sleep(2000L)
}

case class ManageAccount(accountNumber: String)

class AccountService extends Actor {
  var account: ActorRef = _

  def receive = {
  	case manage: ManageAccount =>
  	  account = context.actorOf(Props[Account], manage.accountNumber)
  	  account ! OpenAccount(manage.accountNumber, 100)
  	case (state, event: AccountOpened) =>
  	  println(s"Event: $event")
  	  println(s"State: $state")
  	  account ! DepositFunds(50)
  	case (state, event: FundsDeposited) =>
  	  println(s"Event: $event")
  	  println(s"State: $state")
  	  account ! WithdrawFunds(75)
    case (state, event: FundsWithdrawn) =>
      println(s"Event: $event")
  	  println(s"State: $state")
  }
}
