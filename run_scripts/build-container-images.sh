#!/bin/bash

# First setup the environment
THIS_SCRIPT=$(basename "$0")
SCRIPT_PATH=$(realpath $THIS_SCRIPT)
SCRIPT_DIR=$(dirname $SCRIPT_PATH)
DOCKERFILE_DIR="${SCRIPT_DIR}/../container_configs/docker_files"
SRC_DIR="${SCRIPT_DIR}/../ps_bench"

# Build the containers
cd $SRC_DIR

echo "==== Building EMQX Broker ===="
docker build -t emqx-with-exporter -f $DOCKERFILE_DIR/Dockerfile.emqx .

echo "==== Building Mochi Broker ===="
docker build -t mochi-with-exporter -f $DOCKERFILE_DIR/Dockerfile.mochi .

echo "==== Building Mosquitto Broker ===="
docker build -t mosquitto-with-exporter -f $DOCKERFILE_DIR/Dockerfile.mosquitto .

echo "==== Building NanoMQ Broker ===="
docker build -t nanomq-with-exporter -f $DOCKERFILE_DIR/Dockerfile.nanomq .

echo "==== Building VerneMQ Broker ===="
docker build -t vernemq-with-exporter -f $DOCKERFILE_DIR/Dockerfile.vernemq .

echo "==== Building Benchmark Runner ===="
docker build -t psmark-runner -f $DOCKERFILE_DIR/Dockerfile .

echo "==== Container Images Built! ===="