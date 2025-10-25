# rolang

a small Functional Reactive Programming language friend (tya2104@columbia.edu) and I wrote for a programming languaged and translators class based on a paper about reactive programming. The main idea is you write relationships that should always be true, and the runtime keeps them true when inputs change --it reacts to change.

## quick example

instead of polling in a loop like this:

```python
while True:
    distance = read_sensor()
    if distance < 0.5:
        stop_motor()
    sleep(0.01)
```

you just write the relationship:

```rolang
signal distance : float;

let should_stop = distance < 0.5;
```

and when distance changes, should_stop updates automatically. no loop needed.

## features

- signals (external inputs that change over time)
- prev operator (get previous value)
- window operator (last N values)
- streams (periodic blocks with local state)
- pattern matching
- type inference
- only recomputes what actually changed (forward cone optimization)

## building

needs OCaml installed:

```bash
brew install ocaml  # on mac
make all
```

this builds three executables:
- `test_frontend` - shows lexer/parser/type checker output
- `rolang_run` - runs programs in simulation mode
- `rolang_event` - event-driven execution (uses Unix.select)

## running the example

```bash
# see the full analysis
./test_frontend examples/demo.rol

# run simulation
./rolang_run examples/demo.rol
```
## how it works

when you write `let stop = distance < 0.5`, the compiler:
1. builds a dependency graph (stop depends on distance)
2. figures out topological order
3. when distance changes, computes forward cone (all nodes reachable from distance)
4. evaluates only those nodes

so if you have 100 expressions but only 10 depend on distance, only those 10 recompute when distance changes. everything else stays idle.
the event-driven runtime uses Unix.select so it sleeps until input arrives - no busy waiting.

## language syntax

looks like OCaml:

```rolang
(* type definitions *)
type Mode = Auto | Manual | Emergency;

(* signals are external inputs *)
signal sensor : float;

(* pure expressions *)
let is_safe = sensor > 0.5;

(* temporal operators *)
let prev_value = prev sensor;
let last_10 = window sensor 10;

(* streams for periodic things *)
let heartbeat : signal int =
  stream [
    start n = 0,
    n <- n + 1,
    emit n
  ] every 20ms;

(* pattern matching *)
let mode_str = match mode with
  | Auto -> "automatic"
  | Manual -> "manual"
  | Emergency -> "emergency";
```

## implementation

Wrote this in OCaml.
