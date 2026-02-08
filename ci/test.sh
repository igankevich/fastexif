#!/bin/sh

main() {
    set -ex
    workdir="$(mktemp -d)"
    trap cleanup EXIT
    cargo_clippy
    cargo_build
}

cargo_clippy() {
    cargo clippy --workspace --quiet --all-features --all-targets -- --deny warnings
}

cargo_build() {
    cargo build --no-default-features
    cargo build --no-default-features --features std
    cargo build --no-default-features --features serde
    cargo build --no-default-features --features apple
    cargo build --no-default-features --features std,serde
    cargo build --no-default-features --features std,apple
}

cleanup() {
    rm -rf "$workdir"
}

main "$@"
