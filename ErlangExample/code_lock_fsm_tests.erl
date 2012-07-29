%% Author: theburningmonk
%% Created: 28 Jul 2012
%% Description: simple unit tests for the code_lock_fsm module
-module(code_lock_fsm_tests).
-import(code_lock_fsm).

-export([run_tests/0]).

run_tests() ->
	run_test("given a locked fsm when the user hasnt entered anything its state should be blank", 
			 fun given_a_locked_fsm_when_the_user_hasnt_entered_anything_its_state_should_be_blank/0,
			 true),
	
	run_test("given a locked fsm when the user enters the first digit it remains locked",
			 fun given_a_locked_fsm_when_the_user_enters_the_first_digit_it_remains_locked/0,
			 true),
	
	run_test("given a locked fsm when the user enters an incorrect code then the state is reset",
			 fun given_a_locked_fsm_when_the_user_enters_an_incorrect_code_then_the_state_is_reset/0,
			 true),
	
	run_test("given a locked fsm when the user enters the correct code then the state is changed to open",
			 fun given_a_locked_fsm_when_the_user_enters_the_correct_code_then_the_state_is_changed_to_open/0,
			 true),
	
	run_test("given a locked fsm when the user calls close it should terminate the fsm",
			 fun given_a_locked_fsm_when_the_user_calls_close_it_should_terminate_the_fsm/0,
			 false),
	
	run_test("given an open fsm when 3 seconds have passed it should revert to locked state",
			 fun given_an_open_fsm_when_3_seconds_have_passed_it_should_revert_to_locked_state/0,
			 true),
	
	run_test("given an open fsm when the user calls close it should terminate the fsm",
		 	 fun given_an_open_fsm_when_the_user_calls_close_it_should_terminate_the_fsm/0,
			 false).

%%% Given a locked FSM
given_a_locked_fsm_when_the_user_hasnt_entered_anything_its_state_should_be_blank() ->
	start_code_lock(),
	case code_lock_fsm:get_state() of
		{ locked, [] } -> ok;
		Reply -> { failed, { expected, { locked, [] } }, { actual, Reply } } 
	end.

given_a_locked_fsm_when_the_user_enters_the_first_digit_it_remains_locked() ->
	start_code_lock(),
	code_lock_fsm:button(1),
	case code_lock_fsm:get_state() of
		{ locked, [1] } -> ok;
		Reply -> { failed, { expected, { locked, [1] } }, { actual, Reply } } 
	end.

given_a_locked_fsm_when_the_user_enters_an_incorrect_code_then_the_state_is_reset() ->
	start_code_lock(),
	code_lock_fsm:button(1),
	code_lock_fsm:button(2),
	code_lock_fsm:button(3),
	code_lock_fsm:button(5),
	case code_lock_fsm:get_state() of
		{ locked, [] } -> ok;
		Reply -> { failed, { expected, { locked, [] } }, { actual, Reply } } 
	end.

given_a_locked_fsm_when_the_user_enters_the_correct_code_then_the_state_is_changed_to_open() ->
	code_lock_fsm:start_link([1, 2, 3, 4]),
	code_lock_fsm:button(1),
	code_lock_fsm:button(2),
	code_lock_fsm:button(3),
	code_lock_fsm:button(4),
	case code_lock_fsm:get_state() of
		open -> ok;
		Reply -> { failed, { expected, { open } }, { actual, Reply } } 
	end.

given_a_locked_fsm_when_the_user_calls_close_it_should_terminate_the_fsm() ->
	code_lock_fsm:start_link([1, 2, 3, 4]),
	code_lock_fsm:button(1),
	try
		code_lock_fsm:stop()
	catch 
		exit: { normal, _ } -> ok
	end,

	case code_lock_fsm:get_state() of
		closed -> ok;
		Reply -> { failed, { expected, closed }, { actual, Reply } } 
	end.

%%% Given an open FSM
given_an_open_fsm_when_3_seconds_have_passed_it_should_revert_to_locked_state() ->
	open_code_lock(),
	timer:sleep(3500),
	case code_lock_fsm:get_state() of
		{ locked, [] } -> ok;
		Reply -> { failed, { expected, { locked, [] } }, { actual, Reply } } 
	end.

given_an_open_fsm_when_the_user_calls_close_it_should_terminate_the_fsm() ->
	open_code_lock(),
	try
		code_lock_fsm:stop()
	catch 
		exit: { normal, _ } -> ok
	end,
	
	case code_lock_fsm:get_state() of
		closed -> ok;
		Reply -> { failed, { expected, closed }, { actual, Reply } } 
	end.

%%% Helper functions
%% Start and link a code lock fsm
start_code_lock() ->
	code_lock_fsm:start_link([1, 2, 3, 4]).

%% Start and link, and input the required sequence to open the code lock
open_code_lock() ->
	code_lock_fsm:start_link([1, 2, 3, 4]),
  	code_lock_fsm:button(1),
	code_lock_fsm:button(2),
	code_lock_fsm:button(3),
	code_lock_fsm:button(4),

	ok.

%% Helper function to orchestrate the running of a test
run_test(TestName, Test, Reset) ->
	io:format("~s:~n", [TestName]),
	try 
		% run the test, if its reply was 'ok' then it passed
		ok = Test(),
		io:format("passed~n", [])
	catch
		% if the test failed for some reason (it returned some atom other than 'ok') then
		% it should come through as 'badmatch'
		error:{ badmatch, [_|_] } -> io:format("failed~n", [])
	end,
	
	% reset the code lock if required
	if Reset -> code_lock_fsm:reset();
	   true -> ok
	end.