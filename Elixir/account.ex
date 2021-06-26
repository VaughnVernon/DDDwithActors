defmodule Account do
  defmodule State do
    @derive {Inspect, only: [:account_number, :balance]}
    defstruct account_number: nil, balance: nil
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
      @derive {Inspect, only: [:account_number, :initial_balance]}
      defstruct account_number: nil, initial_balance: nil
    end

    defmodule FundsDeposited do
      @derive {Inspect, only: [:amount, :balance]}
      defstruct amount: nil, balance: nil
    end

    defmodule FundsWithdrawn do
      @derive {Inspect, only: [:amount, :balance]}
      defstruct amount: nil, balance: nil
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
