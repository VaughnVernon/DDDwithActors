-module(account_runner).
-export([run1/0]).
-import(account, [start/0, process/0]).
-include("account_record.hrl").

receive_result() ->
  receive
    {State, Event} ->
      io:format("Event: ~p~n", [Event]),
      io:format("State: ~p~n", [State]),
      State
  end.

run1() ->
  Self = self(),
  Account = account:start(),
  Account ! {Self, #open_account{account_number="A-1234", initial_balance=100}},
  receive_result(),
  Account ! {Self, #deposit_funds{amount=50}},
  receive_result(),
  Account ! {Self, #withdraw_funds{amount=75}},
  receive_result().
