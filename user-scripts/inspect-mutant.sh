#!/bin/bash

PARENT_DIR="/Users/breitnw/Desktop/research/parent-cat"
PROJECT_NAME="contracts-and-testing"
PROJECT_PATH="$PARENT_DIR/$PROJECT_NAME"
RACKET_BASE="$PARENT_DIR/racket/bin"
RACKET="$RACKET_BASE/racket"

export BENCHMARKS_PATH="gtp-benchmarks/benchmarks_mutation"

# Please, no slashes or spaces in experiment-name
export EXPERIMENT_NAME="mbta_test"

MUTANTS_DIR="$PROJECT_PATH/inspected-mutants"

MUTANT_RUNNER="$PROJECT_PATH/bex/experiment/mutant-runner.rkt"

mkdir -p "$MUTANTS_DIR"

"$RACKET" "$MUTANT_RUNNER" -w "$MUTANTS_DIR" "$@"
