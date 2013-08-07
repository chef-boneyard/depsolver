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

-behaviour(gen_server).

%% API
-export([start_link/1,
         new_problem/2,
         new_problem_with_debug/2,
         add_package/3,
         add_version_constraint/5,
         mark_package_required/1,
         mark_package_suspicious/1,
         mark_package_latest/2,
         solve/0]).
-export([wait_for_reply/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT,5000).
-define(PORT_TIMEOUT, 4000).
-define(RESET_SEQUENCE, "0\n0\n0\n0\n0\n0\nRESET\n").

-record(state, { port, problem, data }).

start_link(Executable) ->
    gen_server:start_link({local, ?SERVER}, depselector, Executable, []).

init(Executable) ->
    process_flag(trap_exit, true),
    case open_port({spawn_executable, Executable}, [exit_status, use_stdio, stream, {line, 1024}]) of
        {error, Reason} ->
            {error, Reason};
        Port ->
            {ok, #state{port = Port}}
    end.

%% TODO for compatibity with pooler, these will need to be modified to send to the provided pid
new_problem_with_debug(ID, NumPackages) ->
    gen_server:call(?SERVER, {new_problem, [ID, NumPackages, 1, 1]}, ?TIMEOUT).

new_problem(ID, NumPackages) ->
    gen_server:call(?SERVER, {new_problem, [ID, NumPackages, 0, 0]}, ?TIMEOUT).

add_package(MinVer, MaxVer, CurVer) ->
    gen_server:call(?SERVER, {update, add_package, [MinVer, MaxVer, CurVer]}, ?TIMEOUT).

add_version_constraint(PackageId, Version, DepPackageId, MinVer, MaxVer) ->
    gen_server:call(?SERVER, {update, add_constraint, [PackageId, Version, DepPackageId, MinVer, MaxVer]}, ?TIMEOUT).

mark_package_required(PackageId) ->
    gen_server:call(?SERVER, {update, mark_required, [PackageId]}, ?TIMEOUT).

mark_package_suspicious(PackageId) ->
    gen_server:call(?SERVER, {update, mark_suspicious, [PackageId]}, ?TIMEOUT).

mark_package_latest(PackageId, Weight) ->
    gen_server:call(?SERVER, {update, mark_latest, [PackageId, Weight]}, ?TIMEOUT).

solve() ->
    gen_server:call(?SERVER, solve, ?TIMEOUT).

handle_call({new_problem, Args}, _From, State) ->
    {reply, ok, State#state{problem = action_to_string(new_problem, Args),
                            data = []} };
handle_call({update, Action, Args}, _From, #state{data = Data} = State) ->
    {reply, ok, State#state{data = [ action_to_string(Action, Args) | Data ]}};
handle_call(solve, _From, State) ->
    R = case do_solve(State) of
        {error, {data, Detail} } ->
            % Fine, we're still in a valid state.
            {reply, {error, Detail}, clean_state(State)};
        {ok, {solution, Any}} ->
            {reply, {solution, Any}, clean_state(State)};
        Other ->
            {stop, Other, State}
    end,
    R;
handle_call(_Other, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({_Port, {exit_status, Status}}, State) ->
    {stop, {port_terminated, Status}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, #state{port = Port}) ->
    % If the port is still open, ensure the process is in (or can get to) a
    % state to notice stdin has been closed, then close it.
    case erlang:port_info(Port) of
        undefined ->
            ok;
        _Other ->
            port_command(Port, ?RESET_SEQUENCE),
            port_close(Port)
    end.

%% Internal impl
%%
clean_state(#state{port = Port}) ->
    #state{port = Port}.

do_solve(#state{port = Port, problem = Problem, data = Data} = _State) ->
    case reset_solver(Port) of
        ok ->
            send_command(Port, Problem),
            Data2 = lists:reverse(Data),
            [send_command(Port, Line) || Line <- Data2],
            send_command(Port, "X"),
            % Now we wait until we either have a solution or don't.
            get_solution(Port);
        error ->
            {error, solver_unavailable}
    end.

reset_solver(Port) ->
    % First clear solver to make sure it's not waiting for any input
    % For example, if a poorly formatted command means it's waiting for the
    % next "int" value as a parameter.  "0" will always be safe to send
    % since it can always be parpsed (int or string), and will be ignored
    % if received as a standalone value.
    port_command(Port, ?RESET_SEQUENCE),
    wait_for_reply(Port, "RESET").

% Wait for the solution
get_solution(Port) ->
    case receive_line(Port) of
        {error, timeout} ->
            {error, timeout};
        "NOSOL" ->
            reply_for_result(solution, none);
        "SOL" ->
            reply_for_result(solution, receive_solution(Port));
        "ERROR" ->
            reply_for_result(data_error, receive_line(Port));
        Other ->
            {error, {unexpected_response, Other}}
    end.

receive_solution(Port) ->
    receive_solution(Port, receive_line(Port)).

receive_solution(_Port, {error, timeout}) ->
     {error, timeout};
receive_solution(Port, Header) ->
    {ok, [DisabledCount], _Ignore} = io_lib:fread("~d", Header),
    Valid = case DisabledCount of
        0 -> valid;
        _ -> invalid
    end,
    case receive_packages(Port, 0, []) of
        {error, timeout} ->
            {error, timeout};
        Packages ->
            { {state, Valid},
              {disabled, DisabledCount},
              {packages, Packages} }
    end.

receive_packages(Port, PackageId, Acc) ->
    case receive_line(Port) of
        {error, Any} ->
            {error, Any};
        "EOS" ->
            lists:reverse(Acc);
        Data ->
            {ok, [Disabled, Version], _Ignore} = io_lib:fread("~d~d", Data),
            receive_packages(Port, PackageId + 1, [{PackageId, Disabled, Version} | Acc])
    end.

% Wait for a specific expected standalone/single-line reply from the port,
% and ignore everything else. A timeout or process gone are the only valid failures.
wait_for_reply(Port, Expected) ->
    case receive_line(Port) of
        {error, timeout} ->
            error;
        {error, gone} ->
            error;
        "RESET" ->
            ok;
        _Other ->
            wait_for_reply(Port, Expected)
    end.

receive_line(Port) ->
    receive_line(Port, []).

% Note that this handles multi-chunk lines "just in case", though our
% protocol does not have any lines that will exceed buffer size.
receive_line(Port, Acc) ->
    receive
        {Port, {data, {eol, Data}}} ->
            lists:flatten(lists:reverse([Data | Acc]));
        {Port, {data, {noeol, Data}}} ->
            receive_line(Port, [Data | Acc]);
        {Port, {exit_status, _Status}} ->
            {error, gone}
    after ?PORT_TIMEOUT ->
        {error, timeout}
    end.

reply_for_result(_, {error, Any}) ->
    {error, Any};
reply_for_result(data_error, Data) ->
    {error, {data, Data}};
reply_for_result(Reply, Data) ->
    {ok, {Reply, Data}}.

action_to_command(new_problem) -> "NEW";
action_to_command(add_package) -> "P";
action_to_command(add_constraint) -> "C";
action_to_command(mark_required) -> "R";
action_to_command(mark_suspicious) -> "S";
action_to_command(mark_latest) -> "L".

send_command(Port, Command) ->
    port_command(Port, Command),
    port_command(Port, "\n").

action_to_string(Action, Args) ->
    Args0 = [ safe_int_to_list(X) || X <- Args ],
    string:join([action_to_command(Action) | Args0], " ").

safe_int_to_list(X) when is_integer(X) ->
    integer_to_list(X);
safe_int_to_list(X) ->
    X.
