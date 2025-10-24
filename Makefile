# Rolang Build System
# Uses OCaml toolchain (ocamllex, ocamlyacc, ocamlc/ocamlopt)

# Configuration
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLFLAGS = -g -w +A-4-42-44-45-48 -I src/frontend -I src/graph -I src/runtime -I src/os

# Source directories
SRC = src/frontend
GRAPH = src/graph
AST = $(SRC)/ast.ml
PARSER_MLY = $(SRC)/parser.mly
SCANNER_MLL = $(SRC)/scanner.mll
PRETTY = $(SRC)/pretty.ml
TYPECHECK = $(SRC)/typecheck.ml
TEST_DRIVER = $(SRC)/test_driver.ml

# Graph source files
GRAPH_SRC = $(GRAPH)/graph.ml
TOPO_SRC = $(GRAPH)/topo.ml
FORWARD_CONE_SRC = $(GRAPH)/forward_cone.ml

# Generated files
PARSER_ML = $(SRC)/parser.ml
PARSER_MLI = $(SRC)/parser.mli
SCANNER_ML = $(SRC)/scanner.ml

# Compiled files
AST_CMO = $(SRC)/ast.cmo
PARSER_CMI = $(SRC)/parser.cmi
PARSER_CMO = $(SRC)/parser.cmo
SCANNER_CMO = $(SRC)/scanner.cmo
PRETTY_CMO = $(SRC)/pretty.cmo
TYPECHECK_CMO = $(SRC)/typecheck.cmo
GRAPH_CMO = $(GRAPH)/graph.cmo
TOPO_CMO = $(GRAPH)/topo.cmo
FORWARD_CONE_CMO = $(GRAPH)/forward_cone.cmo
TEST_DRIVER_CMO = $(SRC)/test_driver.cmo

# Runtime source files
RUNTIME = src/runtime
VALUE_SRC = $(RUNTIME)/value.ml
SIGNAL_REGISTRY_SRC = $(RUNTIME)/signal_registry.ml
TEMPORAL_SRC = $(RUNTIME)/temporal.ml
STREAM_SRC = $(RUNTIME)/stream.ml
EVALUATOR_SRC = $(RUNTIME)/evaluator.ml
RUNTIME_DRIVER_SRC = $(RUNTIME)/runtime_driver.ml
RUNTIME_MAIN_SRC = src/runtime_main.ml

# Runtime compiled files
VALUE_CMO = $(RUNTIME)/value.cmo
SIGNAL_REGISTRY_CMO = $(RUNTIME)/signal_registry.cmo
TEMPORAL_CMO = $(RUNTIME)/temporal.cmo
STREAM_CMO = $(RUNTIME)/stream.cmo
EVALUATOR_CMO = $(RUNTIME)/evaluator.cmo
RUNTIME_DRIVER_CMO = $(RUNTIME)/runtime_driver.cmo
RUNTIME_MAIN_CMO = src/runtime_main.cmo

# OS source files
OS = src/os
UNIX_IO_SRC = $(OS)/unix_io.ml
EVENT_RUNTIME_SRC = $(OS)/event_runtime.ml
EVENT_MAIN_SRC = src/event_main.ml

# OS compiled files
UNIX_IO_CMO = $(OS)/unix_io.cmo
EVENT_RUNTIME_CMO = $(OS)/event_runtime.cmo
EVENT_MAIN_CMO = src/event_main.cmo

# Executables
TEST_EXEC = test_frontend
RUNTIME_EXEC = rolang_run
EVENT_EXEC = rolang_event

# Default target
.PHONY: all
all: $(TEST_EXEC) $(RUNTIME_EXEC) $(EVENT_EXEC)

# Test executable
$(TEST_EXEC): $(AST_CMO) $(PARSER_CMO) $(SCANNER_CMO) $(PRETTY_CMO) $(TYPECHECK_CMO) \
              $(GRAPH_CMO) $(TOPO_CMO) $(FORWARD_CONE_CMO) $(TEST_DRIVER_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $^

# Runtime executable
$(RUNTIME_EXEC): $(AST_CMO) $(PARSER_CMO) $(SCANNER_CMO) $(TYPECHECK_CMO) \
                 $(GRAPH_CMO) $(TOPO_CMO) $(FORWARD_CONE_CMO) \
                 $(VALUE_CMO) $(SIGNAL_REGISTRY_CMO) $(TEMPORAL_CMO) \
                 $(STREAM_CMO) $(EVALUATOR_CMO) $(RUNTIME_DRIVER_CMO) $(RUNTIME_MAIN_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ unix.cma $^

# Event-driven executable
$(EVENT_EXEC): $(AST_CMO) $(PARSER_CMO) $(SCANNER_CMO) $(TYPECHECK_CMO) \
               $(GRAPH_CMO) $(TOPO_CMO) $(FORWARD_CONE_CMO) \
               $(VALUE_CMO) $(SIGNAL_REGISTRY_CMO) $(TEMPORAL_CMO) \
               $(STREAM_CMO) $(EVALUATOR_CMO) \
               $(UNIX_IO_CMO) $(EVENT_RUNTIME_CMO) $(EVENT_MAIN_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ unix.cma $^

# Generate parser
$(PARSER_ML) $(PARSER_MLI): $(PARSER_MLY)
	$(OCAMLYACC) -b $(SRC)/parser $<

# Generate scanner
$(SCANNER_ML): $(SCANNER_MLL) $(PARSER_CMI)
	$(OCAMLLEX) -o $@ $<

# Compile AST (no dependencies)
$(AST_CMO): $(AST)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile parser interface
$(PARSER_CMI): $(PARSER_MLI) $(AST_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile parser implementation
$(PARSER_CMO): $(PARSER_ML) $(PARSER_CMI)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile scanner
$(SCANNER_CMO): $(SCANNER_ML) $(PARSER_CMI)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile pretty printer
$(PRETTY_CMO): $(PRETTY) $(AST_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile type checker
$(TYPECHECK_CMO): $(TYPECHECK) $(AST_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile graph modules
$(GRAPH_CMO): $(GRAPH_SRC) $(AST_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(TOPO_CMO): $(TOPO_SRC) $(GRAPH_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(FORWARD_CONE_CMO): $(FORWARD_CONE_SRC) $(GRAPH_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile test driver
$(TEST_DRIVER_CMO): $(TEST_DRIVER) $(AST_CMO) $(PARSER_CMI) $(SCANNER_CMO) $(PRETTY_CMO) $(TYPECHECK_CMO) \
                    $(GRAPH_CMO) $(TOPO_CMO) $(FORWARD_CONE_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile runtime modules
$(VALUE_CMO): $(VALUE_SRC) $(AST_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(SIGNAL_REGISTRY_CMO): $(SIGNAL_REGISTRY_SRC) $(VALUE_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(TEMPORAL_CMO): $(TEMPORAL_SRC) $(VALUE_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(STREAM_CMO): $(STREAM_SRC) $(AST_CMO) $(VALUE_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(EVALUATOR_CMO): $(EVALUATOR_SRC) $(AST_CMO) $(VALUE_CMO) $(GRAPH_CMO) \
                  $(SIGNAL_REGISTRY_CMO) $(TEMPORAL_CMO) $(STREAM_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(RUNTIME_DRIVER_CMO): $(RUNTIME_DRIVER_SRC) $(AST_CMO) $(VALUE_CMO) $(GRAPH_CMO) \
                       $(SIGNAL_REGISTRY_CMO) $(STREAM_CMO) $(EVALUATOR_CMO) $(FORWARD_CONE_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(RUNTIME_MAIN_CMO): $(RUNTIME_MAIN_SRC) $(AST_CMO) $(PARSER_CMI) $(SCANNER_CMO) \
                     $(TYPECHECK_CMO) $(GRAPH_CMO) $(TOPO_CMO) $(VALUE_CMO) $(RUNTIME_DRIVER_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile OS modules
$(UNIX_IO_CMO): $(UNIX_IO_SRC) $(VALUE_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(EVENT_RUNTIME_CMO): $(EVENT_RUNTIME_SRC) $(VALUE_CMO) $(GRAPH_CMO) $(SIGNAL_REGISTRY_CMO) \
                      $(EVALUATOR_CMO) $(UNIX_IO_CMO) $(FORWARD_CONE_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

$(EVENT_MAIN_CMO): $(EVENT_MAIN_SRC) $(AST_CMO) $(PARSER_CMI) $(SCANNER_CMO) $(TYPECHECK_CMO) \
                   $(GRAPH_CMO) $(TOPO_CMO) $(EVENT_RUNTIME_CMO) $(UNIX_IO_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Test with comprehensive example
.PHONY: test
test: $(TEST_EXEC)
	@echo "==================================================================="
	@echo "Testing Rolang Frontend"
	@echo "==================================================================="
	./$(TEST_EXEC) examples/comprehensive_test.rol

# Test lexer only
.PHONY: test-lex
test-lex: $(TEST_EXEC)
	./$(TEST_EXEC) examples/comprehensive_test.rol --lex-only

# Clean
.PHONY: clean
clean:
	rm -f $(SRC)/*.cmo $(SRC)/*.cmi $(SRC)/*.cmx $(SRC)/*.o
	rm -f $(GRAPH)/*.cmo $(GRAPH)/*.cmi $(GRAPH)/*.cmx $(GRAPH)/*.o
	rm -f $(RUNTIME)/*.cmo $(RUNTIME)/*.cmi $(RUNTIME)/*.cmx $(RUNTIME)/*.o
	rm -f $(OS)/*.cmo $(OS)/*.cmi $(OS)/*.cmx $(OS)/*.o
	rm -f src/*.cmo src/*.cmi src/*.cmx src/*.o
	rm -f $(PARSER_ML) $(PARSER_MLI) $(SCANNER_ML)
	rm -f $(TEST_EXEC) $(RUNTIME_EXEC) $(EVENT_EXEC)
	rm -f rolang_graph.dot

# Help
.PHONY: help
help:
	@echo "Rolang Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build test executable (default)"
	@echo "  test      - Run comprehensive test"
	@echo "  test-lex  - Test lexer only (show tokens)"
	@echo "  clean     - Remove build artifacts"
	@echo "  help      - Show this help"

.PHONY: show-files
show-files:
	@echo "Source files:"
	@ls -la $(SRC)/*.ml $(SRC)/*.mll $(SRC)/*.mly 2>/dev/null || true
	@echo ""
	@echo "Generated files:"
	@ls -la $(PARSER_ML) $(PARSER_MLI) $(SCANNER_ML) 2>/dev/null || true
	@echo ""
	@echo "Compiled files:"
	@ls -la $(SRC)/*.cmo $(SRC)/*.cmi 2>/dev/null || true
