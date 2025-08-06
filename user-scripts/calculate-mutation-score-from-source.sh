#!/bin/bash

PARENT_DIR="/Users/nhejduk/Research-Local/blgt-parent"
PROJECT_NAME="contracts-and-testing"
PROJECT_PATH="$PARENT_DIR/$PROJECT_NAME"
RACKET_BASE="$PARENT_DIR/racket/bin"
RACKET="$RACKET_BASE/racket"
RACO="$RACKET_BASE/raco"

export BENCHMARKS_PATH="gtp-benchmarks/benchmarks"
export BENCHMARK_NAME="abm_test"

# Please, no slashes or spaces in experiment-name
export EXPERIMENT_NAME="abm"

SOURCE_CODE_BASE="$PARENT_DIR/$BENCHMARKS_PATH/$BENCHMARK_NAME"
SOURCE_CODE_WITH_TESTS="$SOURCE_CODE_BASE/original"
TEST_DIR="$SOURCE_CODE_BASE/tests"
UNTYPED_DIR="$SOURCE_CODE_BASE/untyped"
NUM_CORES=8
DB_DIR="$PROJECT_PATH/bex/dbs/$EXPERIMENT_NAME"

pushd "$PROJECT_PATH/bex" || exit

find "$PROJECT_PATH" -name compiled -type d -prune -exec rm -r '{}' ';'
"$RACO" pkg install --auto "https://github.com/LLazarek/rscript.git"
"$RACO" pkg install

popd || exit


mkdir -p "$DB_DIR"
# In this case, typed is just a copy of untyped, with no additional type information

"$RACO" make "$PROJECT_PATH/bex/util/enumerate-tests-simple.rkt" || exit

"$RACKET" "$PROJECT_PATH/bex/util/enumerate-tests-simple.rkt" --source "$SOURCE_CODE_WITH_TESTS" --test-dir "$TEST_DIR" --untyped-dest-dir "$UNTYPED_DIR" || exit

"$RACO" make "$PROJECT_PATH/bex/orchestration/db-setup/blutil.rkt" || exit
"$RACKET" "$PROJECT_PATH/bex/orchestration/db-setup/blutil.rkt" -j $NUM_CORES --no-viz "$DB_DIR" || exit

"$RACO" make "$PROJECT_PATH/bex/experiment/mutant-factory.rkt" || exit
"$RACKET" -l errortrace -t "$PROJECT_PATH/bex/experiment/mutant-factory.rkt" -- -b "$SOURCE_CODE_BASE" -o "$DB_DIR/data" -n $NUM_CORES -e "$DB_DIR/errs.log" -l "$DB_DIR/$EXPERIMENT_NAME-progress.log" -c "$PROJECT_PATH/bex/configurables/configs/teco-prime.rkt" -P "$DB_DIR/configuration-outcomes/$EXPERIMENT_NAME.rktd" -k || exit

"$RACO" make "$PROJECT_PATH/bex/util/make-test-suite.rkt" || exit
"$RACKET" "$PROJECT_PATH/bex/util/make-test-suite.rkt" || exit

"$RACO" make "$PROJECT_PATH/bex-data-analysis/teco/calculate-teco-score.rkt" || exit
"$RACKET" "$PROJECT_PATH/bex-data-analysis/teco/calculate-teco-score.rkt"
