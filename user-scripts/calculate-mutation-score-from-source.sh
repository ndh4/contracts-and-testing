#!/bin/bash

PARENT_DIR="/Users/nhejduk/Research-Local/blgt-parent"
PROJECT_NAME="contracts-and-testing"
PROJECT_PATH="$PARENT_DIR/$PROJECT_NAME"
RACKET="$PARENT_DIR/racket/bin/racket"

SOURCE_CODE_DIR="benchmarks/forth"
EXPERIMENT_NAME="one_button_run"
SOURCE_CODE_BASE="$PARENT_DIR/gtp-benchmarks/$SOURCE_CODE_DIR"
SOURCE_CODE_WITH_TESTS="$SOURCE_CODE_BASE/original"
TEST_DIR="$SOURCE_CODE_BASE/tests"
UNTYPED_DIR="$SOURCE_CODE_BASE/untyped"
NUM_CORES=8
DB_DIR="$PROJECT_PATH/bex/dbs/$EXPERIMENT_NAME"

mkdir -p "$DB_DIR"
# In this case, typed is just a copy of untyped, with no additional type information
"$RACKET" "$PROJECT_PATH/bex/util/enumerate-tests.rkt" --source "$SOURCE_CODE_WITH_TESTS" --test-dir "$TEST_DIR" --untyped-dest-dir "$UNTYPED_DIR" && \

"$RACKET" "$PROJECT_PATH/bex/orchestration/db-setup/blutil.rkt" -j $NUM_CORES --no-viz "$DB_DIR" && \

"$RACKET" -l errortrace -t "$PROJECT_PATH/bex/experiment/COPY-mutant-factory.rkt" -- -b "$SOURCE_CODE_BASE" -o "$DB_DIR/data" -n $NUM_CORES -e "$DB_DIR/errs.log" -l "$DB_DIR/$EXPERIMENT_NAME-progress.log" -c "$PROJECT_PATH/bex/configurables/configs/teco-prime.rkt" -P "$DB_DIR/configuration-outcomes/$EXPERIMENT_NAME.rktd" -k && \

"$RACKET" "$PROJECT_PATH/bex/util/make-test-suite.rkt" && \

"$RACKET" "$PROJECT_PATH/bex-data-analysis/teco/calculate-teco-score.rkt"
