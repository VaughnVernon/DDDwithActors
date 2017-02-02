using System;
using System.Threading;
using Microsoft.VisualStudio.TestTools.UnitTesting;

using Akka;
using Akka.Actor;

namespace Accounts
{
    [TestClass]
    public class AccountTest
    {
        [TestMethod]
        public void TestAccountActor()
        {
            var system = ActorSystem.Create("acountContext");

            var accountService = system.ActorOf<AccountService>();
            accountService.Tell(new ManageAccount());

            Thread.Sleep(5000);
        }
    }

    public class ManageAccount { }

    public class AccountService : TypedActor,
        IHandle<ManageAccount>,
        IHandle<Tuple<AccountState, AccountOpened>>,
        IHandle<Tuple<AccountState, FundsDeposited>>,
        IHandle<Tuple<AccountState, FundsWithdrawn>>
    {
        private IActorRef account = null;

        public void Handle(ManageAccount message)
        {
            account = Context.ActorOf<Account>("A-1234");
            account.Tell(new OpenAccount("A-1234", 100));
        }

        public void Handle(Tuple<AccountState, AccountOpened> message)
        {
            Console.WriteLine("AccountOpened: State: {0} and Event: {1}", message.Item1, message.Item2);
            account.Tell(new DepositFunds(50));
        }

        public void Handle(Tuple<AccountState, FundsDeposited> message)
        {
            Console.WriteLine("FundsDeposited: State: {0} and Event: {1}", message.Item1, message.Item2);
            account.Tell(new WithdrawFunds(75));
        }

        public void Handle(Tuple<AccountState, FundsWithdrawn> message)
        {
            Console.WriteLine("FundsWithdrawn: State: {0} and Event: {1}", message.Item1, message.Item2);
        }
    }
}
