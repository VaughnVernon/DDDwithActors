-module(account).
-export([start/0, process/1]).
-include("account_record.hrl").

start() ->
  spawn(account, process, [#state{}]).

process(State = #state{balance = Balance}) ->
  receive
    % opening an account
    {Sender, #open_account{account_number = AccountNumber, initial_balance = InitialBalance}} ->
      InitialState = State#state{
        account_number = AccountNumber,
        balance = InitialBalance
      },
      
      Event = #account_opened{
        account_number = AccountNumber,
        initial_balance = InitialBalance
      },
      
      Sender ! {InitialState, Event},
      process(InitialState);
    
    % depositing funds
    {Sender, #deposit_funds{amount = Amount}} ->
      StateAfterDeposit 
        = #state{balance = NewBalance}
        = State#state{
          balance = Balance + Amount
        },
      
      Event = #funds_deposited{
        amount = Amount,
        balance = NewBalance
      },

      Sender ! {StateAfterDeposit, Event},
      process(StateAfterDeposit);
    
    % wihdrawing funds
    {Sender, #withdraw_funds{amount = Amount}} ->
      StateAfterWithdrawal
        = #state{balance = NewBalance}
        = State#state{
          balance = Balance - Amount
      },
      
      Event = #funds_withdrawn{
        amount = Amount,
        balance = NewBalance
      },
      
      Sender ! {StateAfterWithdrawal, Event},
      process(StateAfterWithdrawal);
    
    U = _ ->
      io:format("Unknown command: ~p~n", [U]),
      process(State)
  end.
