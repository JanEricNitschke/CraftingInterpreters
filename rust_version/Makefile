CRAFTING_INTERPRETERS := ../reference/craftinginterpreters
DEBUG_BIN := ../../rust_version/target/debug/rust_version.exe

test_level := chap21_global
sources := src/*.rs Cargo.toml

$(DEBUG_BIN): $(sources)
	cargo build

.PHONY: cargo-test
cargo-test:
	cargo test

.PHONY: craftinginterpreters-test
craftinginterpreters-test:
		cd  $(CRAFTING_INTERPRETERS) && \
		dart tool/bin/test.dart  $(test_level) --interpreter $(DEBUG_BIN)

.PHONY: test
test: cargo-test craftinginterpreters-test