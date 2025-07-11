#!/bin/bash

PARENT_DIR="/Users/nhejduk/Research-Local/blgt-parent"
PROJECT_NAME="contracts-and-testing"
PROJECT_PATH="$PARENT_DIR/$PROJECT_NAME"
RACKET="$PARENT_DIR/racket/bin/racket"

SOURCE_CODE_DIR="benchmarks/forth"
FRIENDLY_NAME="forth"
SOURCE_CODE_BASE="$PARENT_DIR/gtp-benchmarks/$SOURCE_CODE_DIR"
SOURCE_CODE_WITH_TESTS="$SOURCE_CODE_BASE/original"
TEST_DIR="$SOURCE_CODE_BASE/tests"
UNTYPED_DIR="$SOURCE_CODE_BASE/untyped"

mkdir -p "$FRIENDLY_NAME"
# In this case, typed is just a copy of untyped, with no additional type information
"$RACKET" "$PROJECT_PATH/bex/util/enumerate-tests.rkt" --source "$SOURCE_CODE_WITH_TESTS" --test-dir "$TEST_DIR" --untyped-dest-dir "$UNTYPED_DIR"
"$RACKET" "$PROJECT_PATH/bex/orchestration/db-setup/blutil.rkt" -j 8 --no-viz "$FRIENDLY_NAME"
#"$RACKET" -l errortrace -t "$PROJECT_PATH/bex/experiment/COPY-mutant-factory.rkt" -- -b "$SOURCE_CODE_BASE" -o "./$FRIENDLY_NAME/data" -n 8 -e "$FRIENDLY_NAME/errs.log" -l "$FRIENDLY_NAME/$FRIENDLY_NAME-progress.log" -c "$PROJECT_PATH/bex/configurables/configs/teco-prime.rkt" -P "$PROJECT_PATH/bex/dbs/blutil/configuration-outcomes/$FRIENDLY_NAME.rktd" -k