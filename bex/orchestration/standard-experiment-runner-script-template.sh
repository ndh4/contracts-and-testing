#!/usr/bin/env bash

PROJ_PATH="<<project-path>>"

BENCH="$1"
CONFIG_NAME="$2"
RECORD_CHECK_CONFIG_PARITY="$3"
EXPERIMENT_DIR="$4"
CPUS="$5"
CONTRACT_SETTING="$6"

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
    OUTPUT_DIR_NAME="$BENCH/$CONTRACT_SETTING"
fi

KEEP_GOING_FLAG="-k"
if [ "$KEEP_GOING" = "n" ]; then
    KEEP_GOING_FLAG=""
fi


export PLTSTDOUT='debug@factory'
export PLTSTDERR='none'

OUTPUT_DIR=$EXPERIMENT_DIR/experiment-output/$OUTPUT_DIR_NAME
DB_DIR=$EXPERIMENT_DIR/dbs
TEMPORARY_DATA_DIR=$EXPERIMENT_DIR/temporary-data/mutant-factory/$CONFIG_NAME
BENCHMARKS_PATH=gtp-benchmarks/benchmarks

mkdir -p $OUTPUT_DIR
mkdir -p $DATA_DIR

hostname >> $OUTPUT_DIR/$BENCH.log
./racket/bin/racket -l errortrace -t contracts-and-testing/bex/experiment/mutant-factory.rkt -- \
    -x "$EXPERIMENT_DIR" \
    -b "$BENCHMARKS_PATH/$BENCH" \
    -t "$CONTRACT_SETTING" \
    -o "$TEMPORARY_DATA_DIR" \
    -n "$CPUS" \
    -e "$OUTPUT_DIR/errs.log" \
    -c "contracts-and-testing/bex/configurables/configs/$CONFIG_NAME" \
    -m "$OUTPUT_DIR/$BENCH-metadata.rktd" \
    -P "$DB_DIR/configuration-outcomes/$OUTPUT_DIR_NAME.rkt" \
    $KEEP_GOING_FLAG >> $OUTPUT_DIR/$BENCH.log 2>&1
popd > /dev/null
