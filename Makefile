# Simple Makefile for Erlang compilation

# Erlang compiler
ERL = erlc

# Erlang source files
SOURCES = socket_test.erl socket_test2.erl

# Compiled beam files (derived from sources)
BEAMS = $(SOURCES:.erl=.beam)

# Default target
all: $(BEAMS)

# Rule to compile .erl files to .beam files
%.beam: %.erl
	$(ERL) $<

# Clean compiled files
clean:
	rm -f *.beam
	rm -f erl_crash.dump

# Run socket_test with port 9876
run-test1:
	erl -noshell -s socket_test main 9876

# Run socket_test2 with port 9876
run-test2:
	erl -noshell -s socket_test2 main 9876

# Phony targets
.PHONY: all clean run-test1 run-test2

# Help target
help:
	@echo "Available targets:"
	@echo "  all        - Compile all Erlang files"
	@echo "  clean      - Remove compiled files and crash dumps"
	@echo "  run-test1  - Compile and run socket_test with port 9876"
	@echo "  run-test2  - Compile and run socket_test2 with port 9876"
	@echo "  help       - Show this help message"
