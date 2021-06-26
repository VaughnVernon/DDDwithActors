defmodule Receiver do
  def receive_result do
    receive do
      {state, event} ->
        IO.inspect(event)
        IO.inspect(state)
        state
    end
  end
end

account = Account.start()

Account.open(account, account_number: "A-1234", initial_balance: 100)

Receiver.receive_result()

Account.deposit(account, amount: 50)

Receiver.receive_result()

Account.withdraw(account, amount: 75)

Receiver.receive_result()
