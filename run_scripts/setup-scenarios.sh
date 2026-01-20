#!/bin/bash

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