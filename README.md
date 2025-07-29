# IPv4/IPv6 Dual Socket Binding Test

This repository contains both Erlang and C programs to test IPv4 and IPv6 socket binding behavior on different operating systems, specifically examining how the `IPV6_V6ONLY` socket option affects the ability to bind separate sockets to the same port.

**THERE SEEM TO BE SOMETHING WRONG WITH THE ERLANG BEHAVIOUR ON LINUX, or... ???** 
(see the example outputs below)

## Background

It is possible to set up listening sockets on both `0.0.0.0:8080` (for IPv4) and `[::]:8080` (for IPv6) without them conflicting, but the method and necessity depend on the operating system's default behavior.

The potential for conflict is determined by the `IPV6_V6ONLY` socket option, which controls whether an IPv6 socket can also handle IPv4 traffic (a feature known as dual-stack).

### The Role of IPV6_V6ONLY

When you create an IPv6 socket and bind it to the "any" address `[::]`, the `IPV6_V6ONLY` flag dictates its behavior:

- **IPV6_V6ONLY is 0 (false)**: The socket will listen for both IPv6 and IPv4 traffic. In this case, attempting to subsequently bind another socket to `0.0.0.0` on the same port will result in an "address in use" error because the first socket has already claimed that port for all IP traffic.

- **IPV6_V6ONLY is 1 (true)**: The socket will only listen for IPv6 traffic. This allows you to then create a separate IPv4 socket and bind it to `0.0.0.0` on the same port without any conflict.

### Operating System Defaults

The default setting for `IPV6_V6ONLY` varies across different operating systems:

- **Linux**: By default, `IPV6_V6ONLY` is disabled (0). This means a single IPv6 socket bound to `[::]:8080` will handle both IPv4 and IPv6 connections. You would not need a separate IPv4 socket. If you were to create one, you would first need to enable `IPV6_V6ONLY` on the IPv6 socket.

- **Windows**: The default behavior is to have `IPV6_V6ONLY` enabled (1). Therefore, to listen on both IPv4 and IPv6, you must create two separate sockets: one for IPv6 bound to `[::]:8080` and another for IPv4 bound to `0.0.0.0:8080`.

- **macOS and BSDs**: These systems generally follow the Linux model where `IPV6_V6ONLY` is disabled by default. However, on some BSDs, it might be enabled by default.

### How to Ensure Compatibility

To write portable code that works reliably across different systems, the recommended approach is to explicitly set the `IPV6_V6ONLY` flag to 1 on your IPv6 socket. This ensures that the IPv6 socket only handles IPv6 traffic, and you can then proceed to create and bind a separate IPv4 socket.

Here is a conceptual outline of the process:

1. Create an IPv6 socket.
2. Set the `IPV6_V6ONLY` socket option to 1 (true) for this socket.
3. Bind the IPv6 socket to `[::]:8080`.
4. Create an IPv4 socket.
5. Bind the IPv4 socket to `0.0.0.0:8080`.

By following this procedure, you can successfully listen for both IPv4 and IPv6 traffic on the same port without conflicts, regardless of the operating system's default settings.

## Test Programs

This repository includes both Erlang and C test programs that demonstrate this behavior:

### Erlang Implementation

#### socket_test.erl

Tests binding to IPv6 `[::]` and IPv4 `127.0.0.1` on the same port, which is a common scenario where you want a service available locally over IPv4 while being accessible from anywhere over IPv6.

#### socket_test2.erl

Tests binding to IPv6 `[::]` and IPv4 `0.0.0.0` on the same port, examining system default behavior and the effect of the `ipv6_v6only` option.

### C Implementation

#### socket_test2.c

A C implementation that mirrors the behavior of `socket_test2.erl`, performing the same two tests:

1. **System Default Test**: Creates an IPv6 socket bound to `[::]` and then attempts to create an IPv4 socket bound to `0.0.0.0` using system defaults.

2. **IPV6_V6ONLY Test**: Explicitly sets the `IPV6_V6ONLY` socket option to 1 on the IPv6 socket before binding, then attempts to bind the IPv4 socket.

The C version provides identical functionality to the Erlang version but demonstrates the lower-level socket API calls directly.

## How to Build and Run

### Prerequisites

- Erlang/OTP installed on your system
- C compiler (gcc or clang)
- Make (optional, for using the Makefile)

### Compilation

#### Using Make:
```bash
make all
```

#### Manual compilation:

**Erlang:**
```bash
erlc socket_test.erl
erlc socket_test2.erl
```

**C:**
```bash
gcc -Wall -Wextra -std=c99 -o socket_test2 socket_test2.c
```

### Running the Tests

#### Using Make:
```bash
# Run socket_test with port 9876
make run-test1

# Run socket_test2 with port 9876
make run-test2

# Run C socket_test2 with port 9876
make run-test-c

# Run all tests
make test
```

#### Manual execution:

**Erlang:**
```bash
# Run socket_test
erl -noshell -s socket_test main 9876

# Run socket_test2
erl -noshell -s socket_test2 main 9876
```

**C:**
```bash
# Run C socket_test2
./socket_test2 9876
```

Replace `9876` with any high-numbered port you have permission to use.

## Expected Output

You will see one of two outcomes for the first test, depending on your OS configuration:

### Outcome: Conflict

If your system uses dual-stack sockets by default, the first socket will claim the port for both IPv4 and IPv6, causing the second bind to fail.

**Erlang/OTP 27 [erts-15.1.2] Output on Mac (make run-test2):**
```
--- Test: System Default Socket Behavior , Port: 9876 ---
OK: IPv6 socket opened on [::]:9876.
Error: Could not open IPv4 socket, address is already in use.
Result: Conflict! Your system's default is likely a dual-stack socket (ipv6_v6only = false).
--- RESULT: dual_stack_conflict

--- Test: Forcing Sockets with ipv6_v6only ---
OK: IPv6 socket with 'ipv6_v6only' opened on [::]:9876.
OK: IPv4 socket also opened on 0.0.0.0:9876.
Result: Success! ipv6_v6only=true allows separate IPv4 and IPv6 sockets.
--- RESULT: separate_sockets
```

**C Output on Mac (make run-test-c):**
```
=== Test 1: System Default Socket Behavior ===
OK: IPv6 socket opened on [::]:9876
Error: Could not open IPv4 socket, address is already in use.
Result: Conflict! Your system's default is likely a dual-stack socket (IPV6_V6ONLY = 0).

=== Test 2: Forcing Sockets with IPV6_V6ONLY ===
OK: IPv6 socket with 'IPV6_V6ONLY' opened on [::]:9876
OK: IPv4 socket also opened on 0.0.0.0:9876
Result: Success! IPV6_V6ONLY=1 allows separate IPv4 and IPv6 sockets.
```

**Erlang/OTP 28 [erts-16.0.1] Output on Linux (make run-test2):**
```
--- Test: System Default Socket Behavior , Port: 9876 ---
OK: IPv6 socket opened on [::]:9876.
OK: IPv4 socket also opened on 0.0.0.0:9876.
Result: Success! Your system allows separate IPv4 and IPv6 binds by default.
--- RESULT: separate_sockets

--- Test: Forcing Sockets with ipv6_v6only ---
OK: IPv6 socket with 'ipv6_v6only' opened on [::]:9876.
OK: IPv4 socket also opened on 0.0.0.0:9876.
Result: Success! ipv6_v6only=true allows separate IPv4 and IPv6 sockets.
--- RESULT: separate_sockets
```

**C Output on Linux (make run-test-c):**
```
=== Test 1: System Default Socket Behavior ===
OK: IPv6 socket opened on [::]:9876
Error: Could not open IPv4 socket, address is already in use.
Result: Conflict! Your system's default is likely a dual-stack socket (IPV6_V6ONLY = 0).

=== Test 2: Forcing Sockets with IPV6_V6ONLY ===
OK: IPv6 socket with 'IPV6_V6ONLY' opened on [::]:9876
OK: IPv4 socket also opened on 0.0.0.0:9876
Result: Success! IPV6_V6ONLY=1 allows separate IPv4 and IPv6 sockets.
```

**THERE SEEM TO BE SOMETHING WRONG WITH THE ERLANG BEHAVIOUR ON LINUX, or... ???** 


## Makefile Targets

- `make all` or `make` - Compile all Erlang files and C executable
- `make clean` - Remove compiled files and crash dumps
- `make run-test1` - Compile and run socket_test (Erlang) with port 9876
- `make run-test2` - Compile and run socket_test2 (Erlang) with port 9876
- `make run-test-c` - Compile and run socket_test2 (C) with port 9876
- `make test` - Run all tests (Erlang and C)
- `make help` - Show available targets

## Understanding the Results

The test results will help you understand your system's default IPv6 socket behavior:

- **If the first test fails**: Your system uses dual-stack sockets by default (common on Linux)
- **If the first test succeeds**: Your system separates IPv4 and IPv6 by default (common on Windows)
- **The second test should always succeed**: Explicitly setting `ipv6_v6only` ensures portability

This knowledge is crucial for writing network applications that work consistently across different operating systems.
