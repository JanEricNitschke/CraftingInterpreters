CRAFTING_INTERPRETERS := ../reference/craftinginterpreters
DEBUG_BIN := target/debug/rust_version.exe

test_level := chap27_classes
sources := src/*.rs src/compiler/*.rs Cargo.toml

$(DEBUG_BIN): $(sources)
	cargo build

.PHONY: cargo-test
cargo-test:
	cargo test

.PHONY: craftinginterpreters-test
craftinginterpreters-test:
		cd  $(CRAFTING_INTERPRETERS) && \
		dart tool/bin/test.dart  $(test_level) --interpreter ../../rust_version/$(DEBUG_BIN) \
		&& dart tool/bin/test.dart  $(test_level) --interpreter ../../rust_version/$(DEBUG_BIN) --arguments --stress-gc

.PHONY: custom-dart-test
custom-dart-test: $(DEBUG_BIN)
	dart $(CRAFTING_INTERPRETERS)/tool/bin/test.dart clox --interpreter ./$(DEBUG_BIN) \
	&& dart $(CRAFTING_INTERPRETERS)/tool/bin/test.dart clox --interpreter ./$(DEBUG_BIN) --arguments --stress-gc

.PHONY: test
test: cargo-test craftinginterpreters-test custom-dart-test

.PHONE: hyperfine
hyperfine:
	hyperfine --warmup 1 "..\reference\craftinginterpreters\jlox.bat benchmark\fib.lox" "..\reference\craftinginterpreters\clox.exe benchmark\fib.lox" ".\target\release\rust_version.exe benchmark\fib.lox" ".\target\release\rust_version.exe --stress-gc benchmark\fib.lox" "python benchmark\fib.py" "ruby benchmark\fib.rb"