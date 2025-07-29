-module(socket_test).
-export([main/1]).

%% erlc socket_test.erl
%% erl -noshell -s socket_test main 9876

%% @doc Main entry point. Expects the port number as a command-line argument.
main([PortStr]) ->
    io:format( "Starting socket test with port: ~p~n" , [ PortStr ] ),
    try
        Port = list_to_integer( atom_to_list( PortStr ) ) ,
        io:format("--- Test: System Default (Listen on [::] and 127.0.0.1) ---~n"),
        run_default_test(Port),
        io:format("~n--- Test: Forcing Sockets with ipv6_v6only ---~n"),
        run_v6only_test(Port),
        halt(0)
    catch
        error:badarg ->
            io:format(standard_error, "Error: Invalid port number provided.~nUsage: erl -noshell -s socket_test main <port>~n", []),
            halt(1)
    end;
main(_) ->
    io:format(standard_error, "Usage: erl -noshell -s socket_test main <port>~n", []),
    halt(1).

%% @doc Test 1: Attempts to bind to IPv6 [::] then IPv4 127.0.0.1 using system defaults.
run_default_test(Port) ->
    ListenIPv6 =
        try
            {ok, Socket} = gen_tcp:listen(Port, [inet6, {ip, {0,0,0,0,0,0,0,0}}, binary, {active, false}]),
            io:format("OK: IPv6 socket opened on [::]:~p.~n", [Port]),
            Socket
        catch
            Type:Reason ->
                io:format("Error opening initial IPv6 socket: ~p:~p~n", [Type, Reason]),
                exit({ipv6_fail, Reason})
        end,

    try
        % Attempt to bind to the IPv4 loopback address 127.0.0.1
        {ok, ListenIPv4} = gen_tcp:listen(Port, [{ip, {127,0,0,1}}, binary, {active, false}]),
        io:format("OK: IPv4 socket also opened on 127.0.0.1:~p.~n", [Port]),
        io:format("Result: Success! Your system allows separate IPv4 and IPv6 binds by default.~n"),
        gen_tcp:close(ListenIPv4)
    catch
        error:Reason2 ->
            io:format("Error: Could not open IPv4 socket on 127.0.0.1, Reason: ~p~n", [Reason2]),
            io:format("Result: Conflict! Your system's default is likely a dual-stack socket (ipv6_v6only = false).~n")
    end,
    gen_tcp:close(ListenIPv6).

%% @doc Test 2: Explicitly sets {ipv6_v6only, true} to ensure sockets can coexist.
run_v6only_test(Port) ->
    ListenIPv6 =
        try
            % Note the addition of {ipv6_v6only, true}
            SocketOpts = [inet6, {ip, {0,0,0,0,0,0,0,0}}, {ipv6_v6only, true}, binary, {active, false}],
            {ok, Socket} = gen_tcp:listen(Port, SocketOpts),
            io:format("OK: IPv6 socket with 'ipv6_v6only' opened on [::]:~p.~n", [Port]),
            Socket
        catch
            Type:Reason ->
                io:format("Error opening initial IPv6 socket: ~p:~p~n", [Type, Reason]),
                exit({ipv6_fail, Reason})
        end,

    try
        % Attempt to bind to the IPv4 loopback address 127.0.0.1
        {ok, ListenIPv4} = gen_tcp:listen(Port, [{ip, {127,0,0,1}}, binary, {active, false}]),
        io:format("OK: IPv4 socket also opened on 127.0.0.1:~p.~n", [Port]),
        io:format("Result: Success! Sockets coexist when 'ipv6_v6only' is set explicitly.~n"),
        gen_tcp:close(ListenIPv4)
    catch
        error:Reason2 ->
            io:format("Error: Still could not open IPv4 socket. Reason: ~p~n", [Reason2]),
            io:format("Result: This is an unexpected failure.~n")
    end,
    gen_tcp:close(ListenIPv6).
