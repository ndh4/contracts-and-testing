#!/bin/bash

PARENT_PATH="$HOME/Documents/Research-Cloud/teco-parent" # add me!
PROJECT_PATH="$PARENT_PATH/contracts-and-testing"
RACKET="$PARENT_PATH/racket/bin/racket"

# run the setup script for this experiment
#sh "$PROJECT_PATH/bex/setup/install-racket-and-setup.sh" "$PARENT_PATH" "contracts-and-testing/bex/setup/teco-setup-config.rkt" || exit

# Ensure that the experiment name is fresh
exp_name=$("$RACKET" "$PROJECT_PATH/bex/orchestration/print-experiment-name.rkt")
if [[ -d "$PARENT_PATH/experiment-data/dbs/$exp_name" ||
      -d "$PARENT_PATH/experiment-output/$exp_name" ]]; then
    echo "Error: Directory already exists: '$exp_name'" >&2
    exit 1
fi

"$RACKET" "$PROJECT_PATH/bex/util/enumerate-tests.rkt" || exit

mkdir -p "$PARENT_PATH/experiment-output"

"$RACKET" "$PROJECT_PATH/bex/orchestration/db-setup/teco.rkt" --no-viz || exit

"$RACKET" "$PROJECT_PATH/bex/orchestration/orchestrate-experiment.rkt" || exit