-module(account).
-export([start/0, process/1]).
-include("account_record.hrl").

start() ->
  spawn(account, process, [#state{}]).

process(State = #state{}) ->
  receive
    {Sender, Command = #open_account{}} ->
      NewState = State#state{account_number=Command#open_account.account_number, balance=Command#open_account.initial_balance},
      Event = #account_opened{account_number=Command#open_account.account_number, initial_balance=Command#open_account.initial_balance},
      Sender ! {NewState, Event},
      process(NewState);
    
    {Sender, Command = #deposit_funds{}} ->
      NewState = State#state{balance = State#state.balance + Command#deposit_funds.amount},
      Event = #funds_deposited{amount = Command#deposit_funds.amount, balance = NewState#state.balance},
      Sender ! {NewState, Event},
      process(NewState);
    
    {Sender, Command = #withdraw_funds{}} ->
      NewState = State#state{balance = State#state.balance - Command#withdraw_funds.amount},
      Event = #funds_withdrawn{amount = Command#withdraw_funds.amount, balance = NewState#state.balance},
      Sender ! {NewState, Event},
      process(NewState);
    
    U = _ ->
      io:format("Unknown command: ~p~n", [U]),
      process(State)
  end.
