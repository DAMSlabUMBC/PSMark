#!/bin/bash

# First setup the environment
THIS_SCRIPT=$(basename "$0")
SCRIPT_PATH=$(realpath $THIS_SCRIPT)
SCRIPT_DIR=$(dirname $SCRIPT_PATH)
SRC_DIR="${SCRIPT_DIR}/../ps_bench"
cd $SRC_DIR

echo "==== Building EMQX Broker ===="
docker build -t emqx-with-exporter -f $SCRIPT_PATH/docker_files/Dockerfile.emqx .

echo "==== Building Mochi Broker ===="
docker build -t mochi-with-exporter -f $SCRIPT_PATH/docker_files/Dockerfile.mochi .

echo "==== Building Mosquitto Broker ===="
docker build -t mosquitto-with-exporter -f $SCRIPT_PATH/docker_files/Dockerfile.mosquitto .

echo "==== Building NanoMQ Broker ===="
docker build -t nanomq-with-exporter -f $SCRIPT_PATH/docker_files/Dockerfile.nanomq .

echo "==== Building VerneMQ Broker ===="
docker build -t vernemq-with-exporter -f $SCRIPT_PATH/docker_files/Dockerfile.vernemq .

echo "==== Building Benchmark Runner ===="
docker build -t psmark-runner -f $SCRIPT_PATH/docker_files/Dockerfile .

echo "==== Container Images Built! ===="