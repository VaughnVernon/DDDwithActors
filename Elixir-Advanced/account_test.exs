ExUnit.start(autorun: false)

defmodule AccountTest do
  use ExUnit.Case, async: true

  Code.require_file("account.ex")

  alias Account.State
  alias Account.Commands.{OpenAccount, DepositFunds, WithdrawFunds}
  alias Account.Events.{AccountOpened, FundsDeposited, FundsWithdrawn}

  setup :start_supervised_account

  test "account_run.exs works" do
    import ExUnit.CaptureIO

    execute_script = fn ->
      Code.require_file("account_run.exs")
    end

    assert capture_io(execute_script) =~ """
           %Account.Events.AccountOpened{account_number: \"A-1234\", initial_balance: 100}
           %Account.State{account_number: \"A-1234\", balance: 100}
           %Account.Events.FundsDeposited{amount: 50, balance: 150}
           %Account.State{account_number: \"A-1234\", balance: 150}
           %Account.Events.FundsWithdrawn{amount: 75, balance: 75}
           %Account.State{account_number: \"A-1234\", balance: 75}
           """
  end

  test "opening account", %{account_pid: pid} do
    send(pid, {self(), %OpenAccount{account_number: "A-1234", initial_balance: 100}})

    assert_receive {%State{account_number: "A-1234", balance: 100},
                    %AccountOpened{account_number: "A-1234", initial_balance: 100}}
  end

  test "depositing funds", %{account_pid: pid} do
    send(pid, {self(), %OpenAccount{account_number: "A-1234", initial_balance: 100}})
    send(pid, {self(), %DepositFunds{amount: 50}})

    assert_receive {%State{account_number: "A-1234", balance: 150}, %FundsDeposited{amount: 50}}
  end

  test "withdrawing funds", %{account_pid: pid} do
    send(pid, {self(), %OpenAccount{account_number: "A-1234", initial_balance: 100}})
    send(pid, {self(), %DepositFunds{amount: 50}})
    send(pid, {self(), %WithdrawFunds{amount: 75}})

    assert_receive {%State{account_number: "A-1234", balance: 75}, %FundsWithdrawn{amount: 75}}
  end

  def start_supervised_account(_ctx) do
    pid =
      start_supervised!(%{
        # See start_account_link/1 for explanation of
        # this verbose syntax.
        id: Account,
        start: {__MODULE__, :start_account_link, []}
      })

    [account_pid: pid]
  end

  # This is a temporary solution to Account not having
  # a proper child_spec/1 of its own. This will go
  # away when we convert Account to a GenServer.
  def start_account_link do
    pid = Account.start()
    Process.link(pid)
    {:ok, pid}
  end
end

ExUnit.run()
