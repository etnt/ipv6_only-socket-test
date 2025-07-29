-module(socket_test2).

-export([main/1]).

%% erlc socket_test2.erl
%% erl -noshell -s socket_test2 main 9876

%% @doc Main entry point. Expects the port number as a command-line argument.
main( [ PortStr ] ) ->
    io:format( "Starting socket test with port: ~p~n" , [ PortStr ] ),
    try
        Port = list_to_integer( atom_to_list( PortStr ) ) ,
        io:format( "--- Test: System Default Socket Behavior , Port: ~w ---~n" , [Port]),
        io:format( "--- RESULT: ~p~n" , [ run_default_test( Port ) ]),
        io:format( "~n--- Test: Forcing Sockets with ipv6_v6only ---~n", [] ),
        io:format( "--- RESULT: ~p~n" , [ run_v6only_test( Port )]),
        erlang:halt(0)
    catch
        error:badarg ->
            io:format("Error: Invalid port number provided.~nUsage: erl -noshell -s socket_test main <port>~n" , [ ] ) ,
            halt(1)
    end;
main( _ ) ->
 io:format("Usage: erl -noshell -s socket_test main <port>~n" , [ ] ) ,
 halt(1) .

%% @doc Test 1: Attempts to bind to IPv6 then IPv4 using system defaults.
run_default_test(Port) ->
    try
        {ok, ListenIPv6} =
            gen_tcp:listen(Port, [inet6, {ip, {0,0,0,0,0,0,0,0}}, binary, {active, false}]),
            io:format("OK: IPv6 socket opened on [::]:~p.~n", [Port]),
        try
            {ok, ListenIPv4} = gen_tcp:listen(Port, [{ip, {0,0,0,0}}, binary, {active, false}]),
            io:format("OK: IPv4 socket also opened on 0.0.0.0:~p.~n", [Port]),
            io:format("Result: Success! Your system allows separate IPv4 and IPv6 "
                      "binds by default.~n"),
            gen_tcp:close(ListenIPv4)
        catch
            error:eaddrinuse ->
                io:format("Error: Could not open IPv4 socket, address is already in use.~n"),
                io:format("Result: Conflict! Your system's default is likely a dual-stack "
                          "socket (ipv6_v6only = false).~n")
        after
            gen_tcp:close(ListenIPv6)
        end
    catch
        Type:Reason ->
            io:format("Error opening initial IPv6 socket: ~p:~p~n", [Type, Reason]),
            {error, {ipv6_fail, Reason}}
    end.

%% @doc Test 2: Explicitly sets {ipv6_v6only, true} to ensure sockets can coexist.
run_v6only_test(Port) ->
    try
        % Note the addition of {ipv6_v6only, true}
        SocketOpts =
            [inet6, {ip, {0, 0, 0, 0, 0, 0, 0, 0}}, {ipv6_v6only, true}, binary, {active, false}],
        {ok, ListenIPv6} = gen_tcp:listen(Port, SocketOpts),
        io:format("OK: IPv6 socket with 'ipv6_v6only' opened on [::]:~p.~n", [Port]),
        try
            {ok, ListenIPv4} = gen_tcp:listen(Port, [{ip, {0, 0, 0, 0}}, binary, {active, false}]),
            io:format("OK: IPv4 socket also opened on 0.0.0.0:~p.~n", [Port]),
            io:format("Result: Success! Your system allows separate IPv4 and IPv6 "
                      "binds by default.~n"),
            gen_tcp:close(ListenIPv4)
        catch
            error:eaddrinuse ->
                io:format("Error: Could not open IPv4 socket, address is already in use.~n"),
                io:format("Result: Conflict! Your system's default is likely a dual-stack "
                          "socket (ipv6_v6only = false).~n")
        after
            gen_tcp:close(ListenIPv6)
        end
    catch
        Type:Reason ->
            io:format("Error opening initial IPv6 socket: ~p:~p~n", [Type, Reason]),
            {error, {ipv6_fail, Reason}}
    end.
