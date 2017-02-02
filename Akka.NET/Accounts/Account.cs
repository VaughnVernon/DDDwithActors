namespace Accounts
{
    using System;
    using Akka;
    using Akka.Actor;

    // Commands
    public class OpenAccount
    {
        public OpenAccount(string accountNumber, int initialBalance)
        {
            AccountNumber = accountNumber;
            InitialBalance = initialBalance;
        }

        public string AccountNumber { get; private set; }
        public int InitialBalance {  get; private set; }
    }

    public class DepositFunds
    {
        public DepositFunds(int amount)
        {
            Amount = amount;
        }

        public int Amount { get; private set; }
    }

    public class WithdrawFunds
    {
        public WithdrawFunds(int amount)
        {
            Amount = amount;
        }

        public int Amount { get; private set; }
    }

    // Events
    public class AccountOpened
    {
        public AccountOpened(string accountNumber, int initialBalance)
        {
            AccountNumber = accountNumber;
            InitialBalance = initialBalance;
        }

        public string AccountNumber { get; private set; }
        public int InitialBalance { get; private set; }
    }

    public class FundsDeposited
    {
        public FundsDeposited(string accountNumber, int amount)
        {
            AccountNumber = accountNumber;
            Amount = amount;
        }

        public string AccountNumber { get; private set; }
        public int Amount { get; private set; }
    }

    public class FundsWithdrawn
    {
        public FundsWithdrawn(string accountNumber, int amount)
        {
            AccountNumber = accountNumber;
            Amount = amount;
        }

        public string AccountNumber { get; private set; }
        public int Amount { get; private set; }
    }

    public class AccountState
    {
        public AccountState(string accountNumber, int balance)
        {
            AccountNumber = accountNumber;
            Balance = balance;
        }

        internal string AccountNumber { get; private set; }
        internal int Balance { get; private set; }

        internal AccountState FromDeposited(int amount)
        {
            return new AccountState(AccountNumber, Balance + amount);
        }

        internal AccountState FromWithdrawn(int amount)
        {
            return new AccountState(AccountNumber, Balance - amount);
        }
    }

    public class Account : TypedActor, IHandle<OpenAccount>, IHandle<DepositFunds>, IHandle<WithdrawFunds>
    {
        public Account()
        {
        }

        public void Handle(OpenAccount open)
        {
            State = new AccountState(open.AccountNumber, open.InitialBalance);
            var newEvent = new AccountOpened(open.AccountNumber, open.InitialBalance);
            var answer = new Tuple<AccountState, AccountOpened>(State, newEvent);

            Sender.Tell(answer, Sender);
        }

        public void Handle(DepositFunds deposit)
        {
            State = State.FromDeposited(deposit.Amount);
            var newEvent = new FundsDeposited(State.AccountNumber, deposit.Amount);
            var answer = new Tuple<AccountState, FundsDeposited>(State, newEvent);

            Sender.Tell(answer, Sender);
        }

        public void Handle(WithdrawFunds withdraw)
        {
            State = State.FromWithdrawn(withdraw.Amount);
            var newEvent = new FundsWithdrawn(State.AccountNumber, withdraw.Amount);
            var answer = new Tuple<AccountState, FundsWithdrawn>(State, newEvent);

            Sender.Tell(answer, Sender);
        }

        public AccountState State { get; private set; }
    }
}
