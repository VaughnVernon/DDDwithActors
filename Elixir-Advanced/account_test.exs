ExUnit.start(autorun: false)

defmodule AccountTest do
  use ExUnit.Case, async: true

  Code.require_file("account.ex")

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
end

ExUnit.run()
