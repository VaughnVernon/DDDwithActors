defmodule Receiver do
  def receive_result do
    receive do
      {state, event} ->
        IO.puts event
        IO.puts state
        state
    end
  end
end

account = Account.start

send account, {self(), %Account.Commands.OpenAccount{account_number: "A-1234", initial_balance: 100}}

Receiver.receive_result

send account, {self(), %Account.Commands.DepositFunds{amount: 50}}

Receiver.receive_result

send account, {self(), %Account.Commands.WithdrawFunds{amount: 75}}

Receiver.receive_result
