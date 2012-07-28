%% Description: A simple FSM for a code lock with three states
%% based on an example from the OTP system documentation
-module(code_lock_fsm).
-behaviour(gen_fsm).

%% public API functions
-export([start_link/1, stop/0, button/1, get_state/0]).

%% state names
-export([locked/2, open/2]).

%% gen_fsm callback functions
-export([init/1, handle_event/3, handle_sync_event/4, terminate/3]).

%%% API functions

%% gen_fsm:start_link will spawn and register new process and register it
%% so that it's locally known as code_lock
%% it'll then call the init/1 function in the callback module (2nd param)
%% with the argument (3rd param)
start_link(Code) ->
	gen_fsm:start_link({local, code_lock}, ?MODULE, Code, []).

%% this callback function is called by gen_fsm:start_link above if 
%% registration was successful
%% this function is expected to return { ok, StateName, StateData }
%% where StateName is the initial state - locked with initial state
%% data being the tuple { [], lists:reverse(Code) }
%% note: the Code needs to be reversed so that [1, 2, 3, 4] can be entered
%% as 1, 2, 3, 4 as opposed to 4, 3, 2, 1 because of the way the inputs
%% list is built up in the locked state
init(Code) ->
	{ ok, locked, { [], lists:reverse(Code) } }.

get_state() ->
	gen_fsm:send_event(code_lock, get_state).

%% sends a 'button' event to the FSM registered as code_lock, when the event 
%% is received by the FSM, gen_fsm calls StateName(Event, StateData)
%% which is expected to return { next_state, NextStateName, NextStateData }
%% where NextStateName is the next state to move into and NextStateData is
%% the new state data for gen_fsm
button(Digit) ->
	gen_fsm:send_event(code_lock, { button, Digit }).

%% this callback function is called when the FSM is in the 'locked' state
%% and the 'button' event is fired
locked({ button, Digit }, { SoFar, Code }) ->
	case [Digit|SoFar] of
		Code ->
			% if the new digit completes a sequence that matches the code then
			% unlocks the lock
			do_unlock(),
			
			% after the time out (30 seconds) has expired, the callback function
			% StateName(timeout, StateData) is called, which in this calls 
			% will be open(timeout, { [], Code })
			{ next_state, open, { [], Code }, 30000 };
		Incomplete when length(Incomplete) < length(Code) ->
			% if the new digit does not add to a sequence that matches the 
			% length of the code then stay in the 'locked' state but update 
			% the state data to include the new digit
			{ next_state, locked, { Incomplete, Code } };
		_Wrong ->
			% the sequence now matches the code but it's wrong, stay in 'locked'
			% state and clear the digits entered so far 
			{ next_state, locked, { [], Code } }
	end;

locked(get_state, State) ->
	io:format("The code lock is locked, current state ~p", [State]),
	{ next_state, locked, State };

locked(Event, State) ->
	io:format("Unexpected event received in locked state : ~p", [Event]),
	{ next_state, locked, State }.

open(get_state, State) ->
	io:format("The code lock is open, current state ~p", [State]),
	{ next_state, open, State, 30000 };

open(timeout, State) ->
	% after timeout expires in the 'open' state, go into 'locked' state again
	do_lock(),
	{ next_state, locked, State }.

%% gen_fsm:send_all_state_event fires an event to the FSM regardless of its 
%% state and can be handled with a handle_event/3 function defined in the 
%% callback module
stop() ->
	gen_fsm:send_all_state_event(code_lock, stop).

handle_event(stop, _StateName, State) ->
	{ stop, normal, State }.

handle_sync_event(get_state, _From, StateName, State) ->
	{ reply, { StateName, State }, StateName, State }.

terminate(shutdown, _StateName, _State) ->
	do_clean_up(),
	ok.

%%% Private functions
do_unlock() ->
	io:format("The code lock is now unlocked~n").

do_lock() ->
	io:format("The code lock is now locked~n").

do_clean_up() ->
	io:format("The code lock is now termianted...~n").