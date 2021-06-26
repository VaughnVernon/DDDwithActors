defmodule Receiver do
  def receive_result do
    receive do
      {state, event} ->
        IO.inspect(event)
        IO.inspect(state)
        state
    end
  end

  def receive_number_of_results(expected_number_of_results) do
    unless expected_number_of_results == 0 do
      receive_result()
      receive_number_of_results(expected_number_of_results - 1)
    end
  end
end

account = Account.start()

Account.open(account, account_number: "A-1234", initial_balance: 100)

Account.deposit(account, amount: 50)

Account.withdraw(account, amount: 75)

Receiver.receive_number_of_results(3)
