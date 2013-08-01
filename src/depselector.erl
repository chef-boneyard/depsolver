-module(depselector).

-behaviour(gen_server).

%% API
-export([start_link/1,
         new_problem/2,
%         new_problem_with_debug/2,
         add_package/3,
         add_version_constraint/3,
         add_version_constraint/5,
         mark_package_required/1,
         mark_package_suspicious/1,
         mark_package_latest/2,
         solve/0]).
%states
% - ready
% - problem_started
%

% -export([ready

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).
-define(PORT_TIMEOUT, 4000).


-record(state, {port}).

% TODO: make line size configurable ?
% TODO a gen_fsm will probabably be a better fit here, bceause:
% can't add or mark if we haven't started a new problem
% can't request solve if we haven't started
start_link(Executable) ->
    gen_server:start_link({local, ?MODULE}, depselector, Executable, []).

init(Executable) ->
    % Ensure terminate gets invoked via gen_server
    process_flag(trap_exit, true),
    % TODO check results...
    Port = open_port({spawn, Executable}, [stream, {line, 1024}]),
    {ok, #state{port = Port}}.

%new_problem_with_debug(ID, NumPackages) ->
%    gen_server:call(?MODULE, {send, ["NEW", ID, NumPackages, 1, 1]}, ?TIMEOUT).

%new_problem(ID, NumPackages) ->
%    gen_server:call(?MODULE, {send, "NEW", [ID, NumPackages, 0, 0]}, ?TIMEOUT).

new_problem(ID, NumPackages) ->
    gen_server:call(?MODULE, {send, "NEW", [ID, NumPackages]}, ?TIMEOUT).
% returns {package_id, PID}
% TODO we can simplify here and our io interface
% by combinig this with suspicious/required/latest, all in one shot
add_package(MinVer, MaxVer, CurVer) ->
    gen_server:call(?MODULE, {send, "P", [MinVer, MaxVer, CurVer]}, ?TIMEOUT).

add_version_constraint(PackageId, Version, DepPackageId) ->
    add_version_constraint(PackageId, Version, DepPackageId, -2, -2),
    mark_package_suspicious(PackageId).

add_version_constraint(PackageId, Version, DepPackageId, MinVer, MaxVer) ->
    gen_server:call(?MODULE, {send, "C", [PackageId, Version, DepPackageId, MinVer, MaxVer]}, ?TIMEOUT).

mark_package_required(PackageId) ->
    gen_server:call(?MODULE, {send, "R", [PackageId]}, ?TIMEOUT).

mark_package_suspicious(PackageId) ->
    gen_server:call(?MODULE, {send, "S", [PackageId]}, ?TIMEOUT).

mark_package_latest(PackageId, Weight) ->
    gen_server:call(?MODULE, {send, "L", [PackageId, Weight]}, ?TIMEOUT).

solve() ->
    gen_server:call(?MODULE, {send, "X", []}, ?TIMEOUT).

handle_call({send, Command, Args}, _From, State) ->
    send_and_get(Command, Args, State);
handle_call(_Other, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, #state{port = Port} = _State) ->
    port_close(Port).

%%
%% Internal impl
%%

send_and_get(Command, Args, #state{port = Port} = State) ->
    Args0 = [ safe_int_to_list(X) || X <- Args ],
    C = string:join([Command | Args0], " ") ,
    % io:fwrite("SENDING: ~p~n", [C]),
    port_command(Port, C),
    port_command(Port, "\n"),
    case get_response(Port) of
        {error, timeout} ->
     %       io:fwrite("Timeout waiting for port~n"),
            {stop, timeout, State};
        Response ->
            {reply, Response, State}
    end.

safe_int_to_list(X) when is_integer(X) ->
    integer_to_list(X);
safe_int_to_list(X) ->
    X.

% TODO ok, this is looking like way too much time spent concerned with
% {error, timeout} throughout - perhaps an exception woudl be cleaner,
% especially considering timeout should ONLY be a concern in one case
% (solve call)
get_response(Port) ->
    case receive_line(Port) of
        {error, timeout} ->
            {error, timeout};
        "OK" ->
            {ok, ready};
        "NOSOL" ->
            {solution, none};
        "PID" ->
            reply_for_result(package_id, receive_line(Port));
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
    receive
        {Port, {data, {eol, "X"}}} ->
            % do we care about order?
            lists:reverse(Acc);
        {Port, {data, {eol, Data}}} ->
            {ok, [Disabled, Version], _Ignore} = io_lib:fread("~d~d", Data),
            receive_packages(Port, PackageId + 1, [{PackageId, Disabled, Version} | Acc])
    after ?PORT_TIMEOUT ->
        {error, timeout}
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
            receive_line(Port, [Data | Acc])
    after ?PORT_TIMEOUT ->
        {error, timeout}
    end.

reply_for_result(_, {error, timeout}) ->
    {error, timeout};
reply_for_result(Reply, Data) ->
    {ok, {Reply, Data}}.

