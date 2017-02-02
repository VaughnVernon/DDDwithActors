-record(state, {account_number, balance}).

-record(open_account, {account_number, initial_balance}).
-record(deposit_funds, {amount}).
-record(withdraw_funds, {amount}).

-record(account_opened, {account_number, initial_balance}).
-record(funds_deposited, {amount, balance}).
-record(funds_withdrawn, {amount, balance}).

-record(error, {reason}).
