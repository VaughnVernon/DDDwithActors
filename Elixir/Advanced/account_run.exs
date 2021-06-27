with {:ok, account} <- Account.start_link(),
     {:ok, open_events} <- Account.open(account, account_number: "A-1234", initial_balance: 100),
     {:ok, deposit_events} <- Account.deposit(account, amount: 50),
     {:ok, withdraw_events} <- Account.withdraw(account, amount: 75) do
  [open_events, deposit_events, withdraw_events]
  |> List.flatten()
  |> Enum.each(&IO.inspect/1)
end
