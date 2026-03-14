#!/usr/bin/env bash

PROJ_PATH="<<project-path>>"

BENCH="$1"
CONFIG_NAME="$2"
RECORD_CHECK_CONFIG_PARITY="$3"
DB_DIR_NAME="$4"
CPUS="$5"
OUTDIR_NAME="$6"

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

if [ "$DB_DIR_NAME" = "" ]; then
    printf "ERROR missing positional argument 4 specifying db dir name"
    exit 1
fi

if [ "$CPUS" = "" ]; then
    CPUS=30
elif [ "$CPUS" = "decide" ]; then
    CPUS="$(./pick-cpu-count.sh)"
fi

if [ "$OUTDIR_NAME" = "" ]; then
    OUTDIR_NAME="$BENCH"
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
OUTDIR=experiment-output/$OUTDIR_NAME
DB_DIR=experiment-data/dbs/$DB_DIR_NAME
DATA_DIR=experiment-data/results/$CONFIG_NAME
BENCHMARKS_PATH=gtp-benchmarks/benchmarks

mkdir -p $OUTDIR
mkdir -p $DATA_DIR

hostname >> $OUTDIR/$BENCH.log
./racket/bin/racket -l errortrace -t contracts-and-testing/bex/experiment/mutant-factory.rkt -- \
    -b "$BENCHMARKS_PATH/$BENCH" \
    -o "$DATA_DIR" \
    -n "$CPUS" \
    -e "$OUTDIR/errs.log" \
    -c "contracts-and-testing/bex/configurables/configs/$CONFIG_NAME" \
    -m "$OUTDIR/$BENCH-metadata.rktd" \
    -P "$DB_DIR/configuration-outcomes/$OUTDIR_NAME.rkt" \
    $KEEP_GOING_FLAG >> $OUTDIR/$BENCH.log 2>&1
popd > /dev/null
