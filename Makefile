all: build doc

build:
	cargo build

doc:
	cargo doc

test:
	cargo test

.PHONY: all build doc test
