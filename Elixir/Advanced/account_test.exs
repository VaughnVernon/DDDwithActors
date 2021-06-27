ExUnit.start(autorun: false)

defmodule AccountTest do
  use ExUnit.Case, async: true

  Code.require_file("account.ex")

  setup :start_supervised_account

  test "account_run.exs works" do
    import ExUnit.CaptureIO

    execute_script = fn ->
      Code.require_file("account_run.exs")
    end

    assert capture_io(execute_script) =~ """
           %Account.Events.AccountOpened{account_number: \"A-1234\", initial_balance: 100}
           %Account.Events.FundsDeposited{
             account_number: \"A-1234\",
             amount: 50,
             new_balance: 150
           }
           %Account.Events.FundsWithdrawn{
             account_number: \"A-1234\",
             amount: 75,
             new_balance: 75
           }
           """
  end

  test "opening account", %{account_pid: pid} do
    {:ok, [opened]} = Account.open(pid, account_number: "A-1234", initial_balance: 100)
    assert %Account.Events.AccountOpened{account_number: "A-1234", initial_balance: 100} = opened
  end

  test "depositing funds", %{account_pid: pid} do
    {:ok, _} = Account.open(pid, account_number: "A-1234", initial_balance: 100)
    {:ok, [deposited]} = Account.deposit(pid, amount: 50)
    assert %Account.Events.FundsDeposited{amount: 50} = deposited
  end

  test "withdrawing funds", %{account_pid: pid} do
    {:ok, _} = Account.open(pid, account_number: "A-1234", initial_balance: 100)
    {:ok, _} = Account.deposit(pid, amount: 50)
    {:ok, [withdrawn]} = Account.withdraw(pid, amount: 75)
    assert %Account.Events.FundsWithdrawn{amount: 75} = withdrawn
  end

  test "attempting to withdraw from empty account", %{account_pid: pid} do
    {:ok, _} = Account.open(pid, account_number: "A-1234", initial_balance: 0)
    assert {:error, :insufficient_funds} = Account.withdraw(pid, amount: 50)
  end

  def start_supervised_account(_ctx) do
    [account_pid: start_supervised!(Account)]
  end
end

ExUnit.run()
