defmodule Account do
  defmodule State do
    defstruct account_number: nil, balance: nil

    defimpl String.Chars, for: State do
      def to_string(state) do
        "Account.State account_number: #{state.account_number}, balance: #{state.balance}"
      end
    end
  end

  defmodule Commands do
    defmodule OpenAccount do
      defstruct account_number: nil, initial_balance: nil
    end

    defmodule DepositFunds do
      defstruct amount: nil
    end

    defmodule WithdrawFunds do
      defstruct amount: nil
    end
  end

  alias Commands.{OpenAccount, DepositFunds, WithdrawFunds}

  defmodule Events do
    defmodule AccountOpened do
      defstruct account_number: nil, initial_balance: nil
    end

    defimpl String.Chars, for: AccountOpened do
      def to_string(event) do
        "Account.AccountOpened account_number: #{event.account_number}, initial_balance: #{event.initial_balance}"
      end
    end

    defmodule FundsDeposited do
      defstruct amount: nil, balance: nil
    end

    defimpl String.Chars, for: FundsDeposited do
      def to_string(event) do
        "Account.FundsDeposited amount: #{event.amount}, balance: #{event.balance}"
      end
    end

    defmodule FundsWithdrawn do
      defstruct amount: nil, balance: nil
    end

    defimpl String.Chars, for: FundsWithdrawn do
      def to_string(event) do
        "Account.FundsWithdrawn amount: #{event.amount}, balance: #{event.balance}"
      end
    end
  end

  alias Events.{AccountOpened, FundsDeposited, FundsWithdrawn}

  def start do
    spawn(__MODULE__, :process, [%State{}])
  end

  def process(state) do
    receive do
      {sender, command} ->
        {newState, event} = handle(state, command)
        send(sender, {newState, event})
        process(newState)
    end
  end

  def handle(%State{} = state, %OpenAccount{} = open) do
    state = %State{state | account_number: open.account_number, balance: open.initial_balance}

    event = %AccountOpened{
      account_number: open.account_number,
      initial_balance: open.initial_balance
    }

    {state, event}
  end

  def handle(%State{} = state, %DepositFunds{} = deposit) do
    state = %State{state | balance: state.balance + deposit.amount}
    event = %FundsDeposited{amount: deposit.amount, balance: state.balance}
    {state, event}
  end

  def handle(%State{} = state, %WithdrawFunds{} = withdraw) do
    state = %State{state | balance: state.balance - withdraw.amount}
    event = %FundsWithdrawn{amount: withdraw.amount, balance: state.balance}
    {state, event}
  end
end
