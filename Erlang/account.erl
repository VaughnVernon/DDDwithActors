-module(account).
-export([start/0, process/1]).
-include("account_record.hrl").

start() ->
  spawn(account, process, [#state{}]).

process(State = #state{balance = Balance}) ->
  receive
    {Sender, #open_account{account_number = AccountNumber, initial_balance = InitialBalance}} ->
      NewState = State#state{account_number=AccountNumber, balance=InitialBalance},
      Event = #account_opened{account_number=AccountNumber, initial_balance=InitialBalance},
      Sender ! {NewState, Event},
      process(NewState);
    
    {Sender, #deposit_funds{amount = Amount}} ->
      NewState = #state{balance = NewBalance} = State#state{balance = Balance + Amount},
      Event = #funds_deposited{amount = Amount, balance = NewBalance},
      Sender ! {NewState, Event},
      process(NewState);
    
    {Sender, #withdraw_funds{amount = Amount}} ->
      NewState = #state{balance = NewBalance} = State#state{balance = Balance - Amount},
      Event = #funds_withdrawn{amount = Amount, balance = NewBalance},
      Sender ! {NewState, Event},
      process(NewState);
    
    U = _ ->
      io:format("Unknown command: ~p~n", [U]),
      process(State)
  end.
