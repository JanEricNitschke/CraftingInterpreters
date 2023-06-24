CRAFTING_INTERPRETERS := ../reference/craftinginterpreters
DEBUG_BIN := target/debug/rust_version.exe

test_level := chap22_local
sources := src/*.rs Cargo.toml

$(DEBUG_BIN): $(sources)
	cargo build

.PHONY: cargo-test
cargo-test:
	cargo test

.PHONY: craftinginterpreters-test
craftinginterpreters-test:
		cd  $(CRAFTING_INTERPRETERS) && \
		dart tool/bin/test.dart  $(test_level) --interpreter ../../rust_version/$(DEBUG_BIN)

.PHONY: custom-dart-test
custom-dart-test: $(DEBUG_BIN)
	dart $(CRAFTING_INTERPRETERS)/tool/bin/test.dart clox --interpreter ./$(DEBUG_BIN)

.PHONY: test
test: cargo-test craftinginterpreters-test custom-dart-test