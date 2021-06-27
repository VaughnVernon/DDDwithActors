defmodule Account do
  defmodule State do
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
      @derive {Inspect, only: [:account_number, :amount, :new_balance]}
      defstruct account_number: nil, amount: nil, new_balance: nil
    end

    defmodule FundsWithdrawn do
      @derive {Inspect, only: [:account_number, :amount, :new_balance]}
      defstruct account_number: nil, amount: nil, new_balance: nil
    end
  end

  alias Events.{AccountOpened, FundsDeposited, FundsWithdrawn}

  use GenServer

  # Client (Public API)

  def start_link(init_arg \\ nil) do
    GenServer.start_link(__MODULE__, init_arg)
  end

  def open(pid, fields) do
    GenServer.call(pid, {:handle, struct!(OpenAccount, fields)})
  end

  def deposit(pid, fields) do
    GenServer.call(pid, {:handle, struct!(DepositFunds, fields)})
  end

  def withdraw(pid, fields) do
    GenServer.call(pid, {:handle, struct!(WithdrawFunds, fields)})
  end

  # Server (process implementation)

  def init(_) do
    {:ok, %State{}}
  end

  def handle_call({:handle, command}, _from, state) do
    case handle_command(command, state) do
      {:error, reason} ->
        {:reply, {:error, reason}, state}

      handle_result ->
        emitted_events = List.wrap(handle_result)
        new_state = Enum.reduce(emitted_events, state, &apply_event/2)
        {:reply, {:ok, emitted_events}, new_state}
    end
  end

  def handle_command(%OpenAccount{} = open, %State{}) do
    %AccountOpened{
      account_number: open.account_number,
      initial_balance: open.initial_balance
    }
  end

  def handle_command(%DepositFunds{} = deposit, %State{} = state) do
    %FundsDeposited{
      account_number: state.account_number,
      amount: deposit.amount,
      new_balance: state.balance + deposit.amount
    }
  end

  def handle_command(%WithdrawFunds{} = withdraw, %State{} = state) do
    event = %FundsWithdrawn{
      account_number: state.account_number,
      amount: withdraw.amount,
      new_balance: state.balance - withdraw.amount
    }

    cond do
      event.new_balance < 0 ->
        {:error, :insufficient_funds}

      :otherwise ->
        event
    end
  end

  def apply_event(%AccountOpened{} = opened, %State{} = state) do
    %State{state | account_number: opened.account_number, balance: opened.initial_balance}
  end

  def apply_event(%FundsDeposited{} = deposited, %State{} = state) do
    %State{state | balance: state.balance + deposited.amount}
  end

  def apply_event(%FundsWithdrawn{} = withdrawn, %State{} = state) do
    %State{state | balance: state.balance - withdrawn.amount}
  end
end
