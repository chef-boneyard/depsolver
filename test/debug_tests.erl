-module(debug_tests).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

missing_test() ->
    %% noapp is missing, but referenced.
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [
                                                                  %% direct dep on noapp
                                                                  {"0.1", [{noapp, "0.1", '>='}]},
                                                                  %% exact dep on app2 which depends on noapp
                                                                  {"0.2", [{app2, "0.1"}]},
                                                                  %% will take any version of app2
                                                                  {"0.3", [{app2, "0.1", '>='}]},
                                                                  {"0.4", [{app2, "0.3", '<='}]}
                                                                 ]},
                                                          
                                                          {app2, [{"0.1", [{noapp, "0.1"}]},
                                                                  {"0.2",[]},
                                                                  {"0.3", [{app3, "0.2"}]},
                                                                  {"0.4", []}]},
                                                          {app3, [{"0.1", []},
                                                                  {"0.2", [{noapp, "0.1"}]},
                                                                  {"0.3", []}]},
                                                          {app4, [{"0.1", [{app3, "100"}]}]}
                                                         ]),

    %% should fail because direct dep not found
    WantFail1 = depsolver:solve(Dom0, [{app1, "0.1"}]),
    ?debugVal(WantFail1),

    %% should fail because dep of dep not found
    WantFail2 = depsolver:solve(Dom0, [{app1, "0.2"}]),
    ?debugFmt("Msg: ~s", [lists:flatten(depsolver:format_error(WantFail2))]),
    ?debugVal(WantFail2),

    %% should fail, pkg exists, but not at version
    WantFail3 = depsolver:solve(Dom0, [{app4, "0.1"}]),
    ?debugFmt("Msg: ~s", [lists:flatten(depsolver:format_error(WantFail3))]),
    ?debugVal(WantFail3),

    %% should end up with app1 0.3, app2 0.4
    WantOK1 = depsolver:solve(Dom0, [{app1, "0.3"}]),
    ?debugVal(WantOK1),
    
    %% should end up with app1 0.4, app2 0.2
    WantOK2 = depsolver:solve(Dom0, [{app1, "0.4"}]),
    ?debugVal(WantOK2),

    ok.
