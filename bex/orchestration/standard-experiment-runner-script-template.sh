#!/usr/bin/env bash

PROJ_PATH="<<project-path>>"

BENCH="$1"
CONFIG_NAME="$2"
RECORD_CHECK_CONFIG_PARITY="$3"
EXPERIMENT_DIR="$4"
CPUS="$5"

KEEP_GOING="y"

pushd "$PROJ_PATH" > /dev/null

if [ "$BENCH" = "" ]; then
    echo "ERROR must provide benchmark to run as argument 1"
    exit 1
fi

if [ "$CONFIG_NAME" = "" ]; then
    echo "ERROR must provide config to run with as argument 2"
    exit 1
fi

PARITY_FLAG=""
if [ "$RECORD_CHECK_CONFIG_PARITY" = "check" ]; then
    PARITY_FLAG="-p"
elif [ "$RECORD_CHECK_CONFIG_PARITY" = "record" ]; then
    PARITY_FLAG="-P"
else
    echo "ERROR missing positional argument 3 specifying config parity mode: 'record' or 'check'"
    exit 1
fi

if [ "$EXPERIMENT_DIR" = "" ]; then
    printf "ERROR missing positional argument 4 specifying the directory where this experiment's output files should be placed"
    exit 1
fi

if [ "$CPUS" = "" ]; then
    CPUS=30
elif [ "$CPUS" = "decide" ]; then
    CPUS="$(./pick-cpu-count.sh)"
fi

if [ "$OUTPUT_DIR_NAME" = "" ]; then
    OUTPUT_DIR_NAME="$BENCH"
fi

KEEP_GOING_FLAG="-k"
if [ "$KEEP_GOING" = "n" ]; then
    KEEP_GOING_FLAG=""
fi


export PLTSTDOUT='debug@factory'
export PLTSTDERR='none'

# TODO in general, I feel like all of these things should go in the
# configurable (or passed in by the experiment manager), not hard-coded
# in the experiment runner script
# TODO sqlite database should not be in db_dir, it should be in output_dir
OUTPUT_DIR=$EXPERIMENT_DIR/experiment-output/$OUTPUT_DIR_NAME
DATA_DIR=$EXPERIMENT_DIR/mutant-runner-results/$CONFIG_NAME
BENCHMARKS_PATH=gtp-benchmarks/benchmarks

mkdir -p $OUTPUT_DIR
mkdir -p $DATA_DIR

hostname >> $OUTPUT_DIR/$BENCH.log
./racket/bin/racket -l errortrace -t contracts-and-testing/bex/experiment/mutant-factory.rkt -- \
    -x "$EXPERIMENT_DIR"
    -b "$BENCHMARKS_PATH/$BENCH" \
    -o "$DATA_DIR" \
    -n "$CPUS" \
    -e "$OUTPUT_DIR/errs.log" \
    -c "contracts-and-testing/bex/configurables/configs/$CONFIG_NAME" \
    -m "$OUTPUT_DIR/$BENCH-metadata.rktd" \
    $KEEP_GOING_FLAG >> $OUTPUT_DIR/$BENCH.log 2>&1
popd > /dev/null
