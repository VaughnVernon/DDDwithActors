-module(account).
-export([start/0, process/1]).
-include("account_record.hrl").

start() ->
  spawn(account, process, [#state{}]).

process(State = #state{}) ->
  receive
    {Sender, Command} ->
      {NewState, Event} = handle(State, Command),
      Sender ! {NewState, Event},
      process(NewState);
    U = _ ->
      io:format("Unknown command: ~p~n", [U]),
      process(State)
  end.

handle(State = #state{}, Command = #open_account{}) ->
  NewState = State#state{account_number=Command#open_account.account_number, balance=Command#open_account.initial_balance},
  Event = #account_opened{account_number=Command#open_account.account_number, initial_balance=Command#open_account.initial_balance},
  {NewState, Event};
handle(State = #state{}, Command = #deposit_funds{}) ->
  NewState = State#state{balance = State#state.balance + Command#deposit_funds.amount},
  Event = #funds_deposited{amount = Command#deposit_funds.amount, balance = NewState#state.balance},
  {NewState, Event};
handle(State = #state{}, Command = #withdraw_funds{}) ->
  NewState = State#state{balance = State#state.balance - Command#withdraw_funds.amount},
  Event = #funds_withdrawn{amount = Command#withdraw_funds.amount, balance = NewState#state.balance},
  {NewState, Event}.
