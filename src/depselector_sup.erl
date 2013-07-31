-module(depselector_sup).
-behavior(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(depselector_sup, []).

init([]) ->
    PrivDir = code:priv_dir(depsolver_app),
    ExePath = filename:join([PrivDir, "solver"]),
    {ok, {{one_for_one, 3, 10},
          [{depselector, {depselector, start_link, [ExePath]},
            permanent, 10, worker, [depselector]}]}}.

