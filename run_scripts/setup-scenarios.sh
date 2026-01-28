#!/bin/bash

# Print help text on error
usage() {
    cat <<EOF

Updates all scenario scripts to use the same time unit duration and ensures broker configurations are in the right place
    Usage: $(basename "${BASH_SOURCE[0]}") <duration> <units>
        - duration: An integer > 0
        - units: One of "hours" "minutes" "seconds"

EOF
    exit
}

# Ensure parameter count is correct
if [ "$#" -ne 2 ]; then
    usage
fi

# Verify interval_size is a number
if [[ "$2" != "hours" ]] && [[ "$2" != "minutes" ]] && [[ "$2" != "seconds" ]]; then
    echo "ERROR: units must be 'hours' 'minutes' or 'seconds'"
fi

if [ $1 -le 0 ]; then
    echo "ERROR: duration must be greater than 0"
    exit
fi

# First setup the paths
SCRIPT_PATH=$(realpath "$0")
SCRIPT_DIR=$(dirname $SCRIPT_PATH)
CONTAINER_CONFIG_DIR="${SCRIPT_DIR}/../container_configs"
SRC_DIR="${SCRIPT_DIR}/../ps_bench"
SCENARIO_DIR="${SRC_DIR}/configs/builtin-test-suites/testcases"

# Set the durations for the scenarios
for file in `find $SCENARIO_DIR -name *.scenario`; do
    sed -i "s/{duration.*/{duration, {$1, $2}},/g" $file
done

# Copy mosquitto.conf to the working dir
cp $CONTAINER_CONFIG_DIR/mosquitto.conf $SRC_DIR/mosquitto.conf