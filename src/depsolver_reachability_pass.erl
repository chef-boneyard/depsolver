-module(depsolver_reachability_pass).

-define(DEBUG(Fmt, Msg), io:format("~s:~p " ++ Fmt, [?FILE, ?LINE] ++ Msg)).

-export([run/1]).

run({_, Tree}) ->
    L = tree_to_list(Tree, []),
    L1 = scrub_tree(L),
    {length(L1), list_to_tree(L1)}.

scrub_tree(Tree) ->
    case scrub_deps(Tree, Tree, []) of
        Tree ->
            Tree;
        [] ->
            Tree;
        Tree1 ->
            scrub_tree(Tree1)
    end.

list_to_tree([{App, Deps, X}]) ->
    {App, Deps, X, nil};
list_to_tree([{App, Deps, X}|T]) ->
    {App, Deps, X, list_to_tree(T)}.

tree_to_list({App, Deps, X, nil}, Accum) ->
    lists:reverse([{App, Deps, X}|Accum]);
tree_to_list({App, Deps, X, Subtree}, Accum) ->
    tree_to_list(Subtree, [{App, Deps, X}|Accum]).

scrub_deps([], _Graph, Accum) ->
    lists:reverse(Accum);
scrub_deps([{App, Vsns, X}|T], Graph, Accum) ->
    case scrub_vsn_deps(Vsns, Graph, []) of
        [] ->
            scrub_deps(T, Graph, Accum);
        Vsns1 ->
            scrub_deps(T, Graph, [{App, Vsns1, X}|Accum])
    end.

scrub_vsn_deps([], _Graph, Accum) ->
    lists:reverse(Accum);
scrub_vsn_deps([{Vsn, []}|T], Graph, Accum) ->
    scrub_vsn_deps(T, Graph, [{Vsn, []}|Accum]);
scrub_vsn_deps([{Vsn, Deps}|T], Graph, Accum) ->
    case scrub_vsn_deps1(Deps, Graph, []) of
        [] ->
            scrub_vsn_deps(T, Graph, Accum);
        Deps1 ->
            scrub_vsn_deps(T, Graph, [{Vsn, Deps1}|Accum])
    end.

scrub_vsn_deps1([], _Graph, Accum) ->
    lists:reverse(Accum);
scrub_vsn_deps1([{Name, Vsn}|T], Graph, Accum) ->
    scrub_vsn_deps1([{Name, Vsn, '='}|T], Graph, Accum);
scrub_vsn_deps1([{Name, FromVsn, ToVsn, between}|T], Graph, Accum) ->
    case satisfiable_constraint(between, Name, FromVsn, ToVsn, Graph) of
        true ->
            scrub_vsn_deps1(T, Graph, [{Name, FromVsn, ToVsn, between}|Accum]);
        false ->
            scrub_vsn_deps1(T, Graph, Accum)
    end;
scrub_vsn_deps1([{Name, Vsn, Constraint}|T], Graph, Accum) ->
    case satisfiable_constraint(Constraint, Name, Vsn, Graph) of
        true ->
            scrub_vsn_deps1(T, Graph, [{Name, Vsn, Constraint}|Accum]);
        false ->
            scrub_vsn_deps1(T, Graph, Accum)
    end;
scrub_vsn_deps1([Name|T], Graph, Accum) ->
    case lists:keymember(Name, 1, Graph) of
        true ->
            scrub_vsn_deps1(T, Graph, [Name|Accum]);
        false ->
            scrub_vsn_deps1(T, Graph, Accum)
    end.



satisfiable_constraint(between, Name, FromVsn, ToVsn, Graph) ->
    case lists:keytake(Name, 1, Graph) of
        false ->
            false;
        {value, {Name, Vsns, _}, _} ->
            eval_constraint(between, FromVsn, ToVsn, Vsns)
    end.

satisfiable_constraint(Constraint, Name, Vsn, Graph) ->
    case lists:keytake(Name, 1, Graph) of
        false ->
            false;
        {value, {Name, Vsns, _}, _} ->
            eval_constraint(constraint_to_op(Constraint), Vsn, Vsns)
    end.

eval_constraint(_Op, _FromVsn, _ToVsn, []) ->
    false;
eval_constraint(between, FromVsn, ToVsn, [{Vsn, _}|T]) ->
    case ec_semver:between(FromVsn, ToVsn, Vsn) of
        true ->
            true;
        false ->
            eval_constraint(between, FromVsn, ToVsn, T)
    end.

eval_constraint(_Op, _Vsn, []) ->
    false;
eval_constraint(Op, Vsn, [{Vsn1, _}|T]) ->
    case ec_semver:Op(Vsn1, Vsn) of
        true ->
            true;
        false ->
            eval_constraint(Op, Vsn, T)
    end.

constraint_to_op('<=') ->
    lte;
constraint_to_op('>=') ->
    gte;
constraint_to_op('=') ->
    eql;
constraint_to_op('~>') ->
    pes;
constraint_to_op(between) ->
    between;
constraint_to_op(lte) ->
    lte;
constraint_to_op(gte) ->
    gte;
constraint_to_op(eql) ->
    eql;
constraint_to_op(pes) ->
    pes.

