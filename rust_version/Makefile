CRAFTING_INTERPRETERS := ../reference/craftinginterpreters
DEBUG_BIN := target/debug/rust_version
REL_BIN := target/release/rust_version

test_level := chap30_optimization
sources := src/*.rs src/compiler/*.rs Cargo.toml

$(DEBUG_BIN): $(sources)
	cargo build

$(REL_BIN): $(sources)
	cargo build --release

.PHONY: cargo-test
cargo-test:
	cargo test


.PHONY: custom-dart-test
custom-dart-test: $(DEBUG_BIN)
	dart $(CRAFTING_INTERPRETERS)/tool/bin/test.dart clox --interpreter ./$(DEBUG_BIN)

.PHONY: stress-gc-test
stress-gc-test: $(DEBUG_BIN)
	dart $(CRAFTING_INTERPRETERS)/tool/bin/test.dart clox --interpreter ./$(DEBUG_BIN) --arguments --stress-gc

.PHONY: test
test: cargo-test custom-dart-test stress-gc-test

.PHONY: benchmark
benchmark: fib-benchmark more-benchmark

.PHONY: more-benchmark
more-benchmark: $(REL_BIN)
	for filename in benchmark/*.lox; do \
		echo $$filename; \
		hyperfine --show-output --warmup 1 ".\\target\\release\\rust_version $$filename" "..\\reference\\craftinginterpreters\\clox $$filename.nom" "..\\reference\\craftinginterpreters\\jlox.bat $$filename.nom"; \
	done

.PHONY: fib-benchmark
fib-benchmark: $(REL_BIN)
	hyperfine --warmup 1 ".\\target\release\\rust_version benchmark\\fib\\fib.lox" "..\\reference\\craftinginterpreters\\clox benchmark\\fib\\fib.lox.nom"   \
	"python benchmark\\fib\\fib.py" "ruby benchmark\\fib\\fib.rb" "..\\reference\\craftinginterpreters\\jlox.bat benchmark\\fib\\fib.lox.nom" \

.PHONY: benchmark-ci
benchmark-ci: fib-benchmark-ci more-benchmark-ci

.PHONY: more-benchmark-ci
more-benchmark-ci: $(REL_BIN)
	for filename in benchmark/*.lox; do \
		echo $$filename; \
		hyperfine --show-output --warmup 1 "./target/release/rust_version $$filename" "../reference/craftinginterpreters/clox $$filename.nom" "../reference/craftinginterpreters/jlox $$filename.nom"; \
	done

.PHONY: fib-benchmark-ci
fib-benchmark-ci: $(REL_BIN)
	hyperfine --warmup 1 "./target/release/rust_version benchmark/fib/fib.lox" "../reference/craftinginterpreters/clox benchmark/fib/fib.lox.nom"   \
	"python benchmark/fib/fib.py" "ruby benchmark/fib/fib.rb" "../reference/craftinginterpreters/jlox benchmark/fib/fib.lox.nom" \
