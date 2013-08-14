%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%%
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @author Marc Paradise <marc@opscode.com>
%%
%%
-module(depselector).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_fsm).
%% State  flow
%% API
-export([start_link/1,
         acquire/0,
         is_ready/0,
         new_problem/2,
         new_problem_with_debug/2,
         add_package/3,
         add_version_constraint/5,
         mark_package_required/1,
         mark_package_suspicious/1,
         mark_package_latest/2,
         solve/0,
         abort/0]).
%% FSM
-export([init/1,
         handle_info/3,
         handle_sync_event/4,
         handle_event/3,
         terminate/3,
         code_change/4]).

%% States
-export([ready/2,
         collecting/2,
         collecting/3,
         solving/2
        ]).

%% Testing interface
-export([force_hang/0,
         force_crash/0,
         force_exit/0,
         force_leak_kill/0]).

-define(SERVER, ?MODULE).
-define(TIMEOUT,5000).
-define(PORT_TIMEOUT, 4000).
% This sequence is intended to first clear any in-flight commands
% such that if the solver is waiting for a specific input (string or numeric)
% it can safely accept them and eat the value.
% 0 received by itself is a no-op.
% Once the expected inputs are cleared, we issue an actual RESET.
-define(RESET_SEQUENCE, "0\n0\n0\n0\n0\n0\nRESET\n").

-record(state, { port :: pid(), % Port process
                 os_pid ::  string(), % port's OS pid
                 reply_to :: pid(), % who to send notification to, when appropriate
                 problem :: string(), % command that initializes a problme in solver
                 problem_params :: [string()], % accumulated commands that  define a problems parameters
                 inbuf :: [string()], % Accumulated input chunks from successive port noeol messages.
                 disable_count :: non_neg_integer(), % Number of packages disabled in the solution-in-progress
                 packages :: [{integer(), integer(), integer()}] }).


start_link(Executable) ->
    gen_fsm:start_link({local, ?SERVER}, depselector, Executable, []).

% The call must ensure that this instance is available for use
% prior to invoking any other calls.  This will ensure that startup has completed, and/or
% any solve in progress is completed.
acquire() ->
    gen_fsm:sync_send_all_state_event(?SERVER, notify_ready).

is_ready() ->
    case gen_fsm:sync_send_all_state_event(?SERVER, curstate) of
        ready -> true;
        _ -> false
    end.

%% TODO for compatibity with pooler, these will need to be modified to use a provided pid
new_problem_with_debug(ID, NumPackages) ->
    do_new_problem(ID, NumPackages, 1, 1).

new_problem(ID, NumPackages) ->
    do_new_problem(ID, NumPackages, 1, 1).

do_new_problem(ID, NumPackages, Stats, Debug) ->
    % let's ensure we're in a valid state.
    % First abort any in-process work - note that
    % if we can't do this (such as for a runaway solve that somehow evaded timeout)
    % we're going to crash the fsm at this point.
    abort(),
    % Now wait for ready state before continuing.
    acquire(),
    % Last, start the problem.
    gen_fsm:send_event(?SERVER, {new_problem, [ID, NumPackages, Stats, Debug]}).

add_package(MinVer, MaxVer, CurVer) ->
    gen_fsm:send_event(?SERVER, {update, add_package, [MinVer, MaxVer, CurVer]}).

add_version_constraint(PackageId, Version, DepPackageId, MinVer, MaxVer) ->
    gen_fsm:send_event(?SERVER, {update, add_constraint, [PackageId, Version, DepPackageId, MinVer, MaxVer]}).

mark_package_required(PackageId) ->
    gen_fsm:send_event(?SERVER, {update, mark_required, [PackageId]}).

mark_package_suspicious(PackageId) ->
    gen_fsm:send_event(?SERVER, {update, mark_suspicious, [PackageId]}).

mark_package_latest(PackageId, Weight) ->
    gen_fsm:send_event(?SERVER, {update, mark_latest, [PackageId, Weight]}).

abort() ->
    % TODO make this sync we we can provide a meaningful reply?
    gen_fsm:send_all_state_event(?SERVER, abort).

solve() ->
    gen_fsm:sync_send_event(?SERVER, solve).

%% Testing interface to induce various error conditions in the external process
force_hang() ->
    gen_fsm:send_event(?SERVER, {test_action, hang}).

force_crash() ->
    gen_fsm:send_event(?SERVER, {test_action, segfault}).

force_exit() ->
    gen_fsm:send_event(?SERVER, {test_action, exit}).

force_leak_kill() ->
    gen_fsm:send_event(?SERVER, {test_action, leak}).

% States
init(Executable) ->
    process_flag(trap_exit, true),
    case open_port({spawn_executable, Executable}, [exit_status, use_stdio, stream, {line, 1024}]) of
        {error, Reason} ->
            {error, Reason};
        Port ->
            % capture pid and persist it.  if we wait until we need it
            % when trying to kill the process, it may not be available to us.
            {os_pid, PID} = erlang:port_info(Port, os_pid),
            {ok, ready, #state{port = Port, os_pid = PID, inbuf = []}}
    end.

ready({new_problem, Args}, State) ->
    NewState = State#state{problem = action_to_string(new_problem, Args), problem_params = []},
    {next_state, collecting, NewState};
ready(timeout, #state{reply_to = undefined} = State) ->
    {next_state, ready, State};
ready(timeout, #state{reply_to = ReplyTo} = State) ->
    gen_fsm:reply(ReplyTo, true),
    {next_state, ready, State#state{reply_to = undefined}}.

collecting({update, Action, Args}, #state{problem_params = Params} = State) ->
    NewState = State#state{problem_params = [ action_to_string(Action, Args) | Params]},
    {next_state, collecting, NewState};
collecting({test_action, Action}, State) ->
    do_test_action(Action, collecting, State).

collecting(solve, From, #state{port = Port} = State) ->
    port_command(Port, ?RESET_SEQUENCE),
    % note that the next_state response means our caller is waiting for us to
    % eventually reply via gen_fsm:reply/2 (or timeout, whichever occurs first)
    {next_state, resetting, State#state{reply_to = From}}.

solving({test_action, Action}, State) ->
    do_test_action(Action, solving, State);
solving(timeout, State) ->
    % This means we never received a solution. Let's shut down,
    % which will notify caller of failure and terminate the solver instance.
    {stop, {error, solution_timeout}, State}.

handle_info({_Port, {data, {eol, Data}}}, StateName, #state{inbuf = Acc} = State) ->
    Line = lists:flatten(lists:reverse([Data | Acc])),
    handle_inbound_data(StateName, Line, State#state{inbuf = []});
handle_info({_Port, {data, {noeol, Data}}}, StateName, #state{inbuf = Acc} = StateData) ->
    % incomplete message, keep accumulating
    {next_state, StateName, StateData#state{inbuf = [Data | Acc]}};
handle_info({_Port, {exit_status, Status}}, _StateName, StateData) ->
    ?debugMsg("Port process terminated."),
    {stop, {port_terminated, Status}, StateData};
handle_info(Msg, StateName, State) ->
    ?debugFmt("[~p] discarding data: ~p", [StateName, Msg]),
    {next_state, StateName, State}.

handle_event(abort, ready, StateData) ->
    {next_state, ready, clean_state(StateData)};
handle_event(abort, collecting, StateData) ->
    {next_state, ready, clean_state(StateData)};
handle_event(abort, StateName, StateData) ->
    % In any other case, we can't process an abort - we're in flight with communications
    % to the solver.
    {next_state, StateName, StateData};
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(curstate, _From, StateName, StateData) ->
    {reply, StateName, StateName, StateData};
handle_sync_event(notify_ready, _From, ready, StateData) ->
    {reply, true, ready, StateData};
handle_sync_event(notify_ready, From, StateName, StateData) ->
    {next_state, StateName, StateData#state{reply_to = From}};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate({port_terminated, _Reason}, _StateName, _State) ->
    ok;
terminate(_Reason, _StateName, #state{port = Port, os_pid = PID}) ->
    try
        close_port(erlang:port_info(Port), Port)
    catch
        _Mod:_Error ->
            ok
    end,
    kill_port_os_proc(PID).

% We are resetting pre-solve and have received confirmation that reset is complete.
handle_inbound_data(resetting, "RESET", #state{port = Port, problem_params = Params, problem = Problem} = State) ->
    send_command(Port, Problem),
    Data2 = lists:reverse(Params),
    [send_command(Port, Line) || Line <- Data2],
    send_command(Port, "X"),
    % Wait for a reply, but for no longer than the specified timeout.
    {next_state, solving, State, ?PORT_TIMEOUT};
handle_inbound_data(ready, "RESET", #state{port = _Port} = State) ->
    % We can receive this if an error occurs in one of the batchced commands we send to solver -
    % we send all commands prior to checking to see if any errors occur.  When that happens,
    % solver sees all commands post-error as unknown input, which causes it to reset state.
    {next_state, ready, State, 0};
handle_inbound_data(solving, "ERROR", #state{port = _Port} = State) ->
    % Wait for the error
    {next_state, solving_error_wait, State};
handle_inbound_data(solving_error_wait, Error, #state{reply_to = ReplyTo} = State) ->
    gen_fsm:reply(ReplyTo, {error, Error}),
    {next_state, ready, clean_state(State), 0};
handle_inbound_data(solving, "SOL", State) ->
    {next_state, solving_wait_header, State#state{packages = []}};
handle_inbound_data(solving, "NOSOL", #state{reply_to = ReplyTo} = State) ->
    gen_fsm:reply(ReplyTo, {solution, none}),
    {next_state, ready, clean_state(State), 0};
handle_inbound_data(solving_wait_header, Data, State) ->
    {ok, [DisabledCount], _Ignore} = io_lib:fread("~d", Data),
    {next_state, solving_wait_packages, State#state{disable_count = DisabledCount}};
handle_inbound_data(solving_wait_packages, "EOS", #state{reply_to = ReplyTo,
                                                         disable_count = DisabledCount,
                                                         packages = Packages } = State) ->
    Valid = case DisabledCount of
        0 -> valid;
        _ -> invalid
    end,
    Solution = { {state, Valid},
                 {disabled, DisabledCount},
                 {packages, lists:reverse(Packages)} },
    gen_fsm:reply(ReplyTo, {solution, Solution}),
    {next_state, ready, clean_state(State), 0};
handle_inbound_data(solving_wait_packages, Data, #state{packages = Packages} = State) ->
    {ok, [Disabled, Version], _Ignore} = io_lib:fread("~d~d", Data),
    Package = {length(Packages), Disabled, Version},
    NewState = State#state{packages = [Package | Packages]},
    {next_state, solving_wait_packages, NewState};
handle_inbound_data(StateName, Data, #state{} = State) ->
    ?debugFmt("Unexpected data ~p in state ~p", [Data, StateName]),
    {next_state, StateName, State}.

%% Internal impl
%%

%% Create a 'clean' #state record, retaining only data required to manage
%% the port.
clean_state(#state{port = Port, os_pid = PID}) ->
    #state{port = Port, os_pid = PID, inbuf = []}.

do_test_action(Action, StateName, #state{port = Port} = State) ->
    send_command(Port, action_to_command(Action)),
    {next_state, StateName, State}.

close_port(undefined, _) -> ok;
close_port(_ ,Port) ->
    port_command(Port, ?RESET_SEQUENCE),
    port_close(Port).

kill_port_os_proc(undefined) -> ok;
kill_port_os_proc(PID) ->
    os:cmd(lists:flatten(["kill -9 ", integer_to_list(PID)])).

send_command(Port, Command) ->
    port_command(Port, Command),
    port_command(Port, "\n").

action_to_string(Action, Args) ->
    Args0 = [ safe_int_to_list(X) || X <- Args ],
    string:join([action_to_command(Action) | Args0], " ").

action_to_command(new_problem) -> "NEW";
action_to_command(add_package) -> "P";
action_to_command(add_constraint) -> "C";
action_to_command(mark_required) -> "R";
action_to_command(mark_suspicious) -> "S";
action_to_command(mark_latest) -> "L";
action_to_command(hang) -> "HANG";
action_to_command(segfault) -> "SEGFAULT";
action_to_command(exit) -> "EXIT";
action_to_command(leak) -> "LEAK".

safe_int_to_list(X) when is_integer(X) ->
    integer_to_list(X);
safe_int_to_list(X) ->
    X.
