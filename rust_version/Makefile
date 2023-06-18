CRAFTING_INTERPRETERS ?= ..\reference\craftinginterpreters
DEBUG_BIN := target\debug\rust_version.exe

test_level := chap19_strings
sources := src/*.rs Cargo.toml

$(DEBUG_BIN): $(sources)
	cargo build

.PHONY: cargo-test
cargo-test:
	cargo test

.PHONY: craftinginterpreters-test
craftinginterpreters-test: $(DEBUG_BIN)
		dart $(CRAFTING_INTERPRETERS)/tool/bin/test.dart jlox $(test_level) --interpreter $(DEBUG_BIN) && \
		dart $(CRAFTING_INTERPRETERS)/tool/bin/test.dart clox $(test_level) --interpreter $(DEBUG_BIN)

.PHONY: test
test: cargo-test craftinginterpreters-test