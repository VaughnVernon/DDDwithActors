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
           #Account.State<account_number: \"A-1234\", balance: 100, ...>
           %Account.Events.FundsDeposited{amount: 50, balance: 150}
           #Account.State<account_number: \"A-1234\", balance: 150, ...>
           %Account.Events.FundsWithdrawn{amount: 75, balance: 75}
           #Account.State<account_number: \"A-1234\", balance: 75, ...>
           """
  end

  test "opening account", %{account_pid: pid} do
    Account.open(pid, account_number: "A-1234", initial_balance: 100)

    assert_receive {%Account.State{account_number: "A-1234", balance: 100},
                    %Account.Events.AccountOpened{account_number: "A-1234", initial_balance: 100}}
  end

  test "depositing funds", %{account_pid: pid} do
    pid
    |> Account.open(account_number: "A-1234", initial_balance: 100)
    |> Account.deposit(amount: 50)

    assert_receive {%Account.State{account_number: "A-1234", balance: 150},
                    %Account.Events.FundsDeposited{amount: 50}}
  end

  test "withdrawing funds", %{account_pid: pid} do
    pid
    |> Account.open(account_number: "A-1234", initial_balance: 100)
    |> Account.deposit(amount: 50)
    |> Account.withdraw(amount: 75)

    assert_receive {%Account.State{account_number: "A-1234", balance: 75},
                    %Account.Events.FundsWithdrawn{amount: 75}}
  end

  def start_supervised_account(_ctx) do
    [account_pid: start_supervised!({Account, self()})]
  end
end

ExUnit.run()
