-module(depselector_sup).
-behavior(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(depselector_sup, []).

init([]) ->
    PrivDir = priv_dir(),
    ExePath = filename:join([PrivDir, "solver"]),
    {ok, {{one_for_one, 3, 10},
          [{depselector, {depselector, start_link, [ExePath]},
            permanent, 10, worker, [depselector]}]}}.

priv_dir() ->
    priv_dir(code:priv_dir(depsolver)).

% TODO this is a hack that I strongly suspect won't work when depsolver
% itself is a dep ...
priv_dir({error, bad_name}) ->
    ModDir = filename:dirname(code:which(depsolver)),
    BaseDir = filename:dirname(ModDir),
    filename:join(BaseDir, "priv");
priv_dir(Other) ->
    Other.
