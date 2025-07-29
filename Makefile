# Simple Makefile for Erlang and C compilation

# Erlang compiler
ERL = erlc

# C compiler
CC = gcc
CFLAGS = -Wall -Wextra -std=c99

# Erlang source files
SOURCES = socket_test.erl socket_test2.erl socket_test3.erl

# Compiled beam files (derived from sources)
BEAMS = $(SOURCES:.erl=.beam)

# C executable
C_EXEC = socket_test2

# Default target
all: $(BEAMS) $(C_EXEC)

# Rule to compile .erl files to .beam files
%.beam: %.erl
	$(ERL) $<

# Rule to compile C executable
$(C_EXEC): socket_test2.c
	$(CC) $(CFLAGS) -o $(C_EXEC) socket_test2.c

# Clean compiled files
clean:
	rm -f *.beam
	rm -f $(C_EXEC)
	rm -f erl_crash.dump

test: all run-test1 run-test2 run-test3 run-test-c

# Run socket_test with port 9876
run-test1:
	erl -noshell -s socket_test main 9876

# Run socket_test2 with port 9876
run-test2:
	erl -noshell -s socket_test2 main 9876

# Run socket_test3 with port 9876
run-test3:
	erl -noshell -s socket_test3 main 9876

# Run C socket_test with port 9876
run-test-c:
	./$(C_EXEC) 9876

# Phony targets
.PHONY: all clean run-test1 run-test2 run-test3 run-test-c test

# Help target
help:
	@echo "Available targets:"
	@echo "  all        - Compile all Erlang files and C executable"
	@echo "  clean      - Remove compiled files and crash dumps"
	@echo "  run-test1  - Compile and run socket_test with port 9876"
	@echo "  run-test2  - Compile and run socket_test2 with port 9876"
	@echo "  run-test3  - Compile and run socket_test3 with port 9876"
	@echo "  run-test-c - Compile and run C socket_test with port 9876"
	@echo "  test       - Run all tests (Erlang and C)"
	@echo "  help       - Show this help message"
