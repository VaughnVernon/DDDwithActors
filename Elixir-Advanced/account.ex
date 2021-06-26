defmodule Account do
  defmodule State do
    @derive {Inspect, only: [:account_number, :balance]}
    defstruct account_number: nil, balance: nil, observer: nil
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

  use GenServer

  # Client (Public API)

  def start_link(observer \\ self()) do
    GenServer.start_link(__MODULE__, observer)
  end

  def open(pid, fields) do
    :ok = GenServer.cast(pid, {:handle, struct!(OpenAccount, fields)})
    pid
  end

  def deposit(pid, fields) do
    :ok = GenServer.cast(pid, {:handle, struct!(DepositFunds, fields)})
    pid
  end

  def withdraw(pid, fields) do
    :ok = GenServer.cast(pid, {:handle, struct!(WithdrawFunds, fields)})
    pid
  end

  # Server (process implementation)

  def init(observer) do
    {:ok, %State{observer: observer}}
  end

  def handle_cast({:handle, command}, state) do
    result = handle(state, command)

    # I assume we are pushing this message back purely for
    # demonstration purposes, so I keep it as is.
    #
    # In real life application I would feel reluctant to
    # have this dependency inside an async (cast) message
    # handler.
    if(state.observer, do: send(state.observer, result))

    {new_state, _event} = result
    {:noreply, new_state}
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
