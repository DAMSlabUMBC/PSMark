#!/usr/bin/env bash
set -euo pipefail

# Run all single-node QoS variation scenarios against each MQTT broker compose stack.

RUN_DIR="`pwd`"
SCRIPT_DIR=$(cd -- "$(dirname "${BASH_SOURCE[0]}")" && pwd)
PS_BENCH_DIR=$(cd -- "${SCRIPT_DIR}/.." && pwd)
COMPOSE_DIR=$(cd -- "$PS_BENCH_DIR/../container_configs/docker_files/compose_yamls" && pwd)
CONTAINER_CONFIG_DIR=$(cd -- "$PS_BENCH_DIR/../container_configs" && pwd)
SCEN_ROOT="${PS_BENCH_DIR}/configs/builtin-test-suites/testcases/high-qos/5-node"
RESULTS_ROOT="${RUN_DIR}/results"
REPEAT_COUNT=${REPEAT_COUNT:-3}
BROKER_LIST=${BROKER_LIST:-emqx,mosquitto,nanomq,vernemq,mochi}
IFS=',' read -r -a BROKERS <<<"${BROKER_LIST}"
SCEN_FILTER=${SCEN_FILTER:-}


if ! command -v docker >/dev/null 2>&1; then
  echo "docker CLI is required but was not found" >&2
  exit 1
fi

if [ ! -d "${SCEN_ROOT}" ]; then
  echo "Scenario root not found: ${SCEN_ROOT}" >&2
  exit 1
fi

mkdir -p "${RESULTS_ROOT}" 2>/dev/null || true

mapfile -t SCENARIO_FILES < <(find "${SCEN_ROOT}" -type f -name 'qossuite_*_mqttv5*.scenario' | sort)
if [ ${#SCENARIO_FILES[@]} -eq 0 ]; then
  echo "No 5-node MQTT QoS scenarios found under ${SCEN_ROOT}" >&2
  exit 1
fi

is_broker_name() {
  local candidate="$1"
  for known in "${BROKERS[@]}"; do
    if [ "${candidate}" = "${known}" ]; then
      return 0
    fi
  done
  return 1
}

move_new_entries() {
  local search_root="$1"
  local destination_root="$2"
  local marker="$3"
  local log_dir="$4"

  [ -d "${search_root}" ] || return 0
  mkdir -p "${destination_root}"

  find "${search_root}" -mindepth 1 -maxdepth 1 -type d -newer "${marker}" -print0 2>/dev/null |
  while IFS= read -r -d '' candidate; do
    local base
    base=$(basename "${candidate}")
    if is_broker_name "${base}"; then
      continue
    fi
    if [ "${base}" = "dds" ]; then
      continue
    fi
    mv "${candidate}" "${destination_root}/${base}"
    mkdir -p "${destination_root}/${base}/broker_logs"
    cp -r $log_dir "${destination_root}/${base}/broker_logs" 2>/dev/null || true
  done
}

current_compose=""
cleanup() {
  if [ -n "${current_compose}" ]; then
    (cd "${PS_BENCH_DIR}" && docker compose -f "${current_compose}" down --remove-orphans >/dev/null 2>&1) || true
  fi
}
trap cleanup EXIT

for scenario_file in "${SCENARIO_FILES[@]}"; do
  scenario_name=$(sed -n "s/{name, \(.*\)},/\1/p" $scenario_file | awk '{$1=$1;print}')

  if [ -n "${SCEN_FILTER}" ] && [[ "${scenario_name}" != *"${SCEN_FILTER}"* ]]; then
    continue
  fi

  scenario_dir=$(dirname "${scenario_file}")
  rel_dir="${scenario_dir#${PS_BENCH_DIR}}"
  container_dir="/app${rel_dir}"

  for broker in "${BROKERS[@]}"; do
    compose_file="$COMPOSE_DIR/docker-compose.mqtt.${broker}.yml"
    if [ ! -f "${compose_file}" ]; then
      echo "Skipping broker ${broker}: missing ${compose_file}" >&2
      continue
    fi
    
    cp $compose_file $RUN_DIR
    run_compose_file="$RUN_DIR/$(basename $compose_file)"
    
    # If broker is mosquitto, we need the config in place
    if [[ "$broker" == "mosquitto" ]]; then
        cp $CONTAINER_CONFIG_DIR/mosquitto.conf $RUN_DIR
    fi
    
    # TODO: Need to change this to not rely on string manipulation
    # Make log directories
    subdir_name=${broker}
    if [[ "$subdir_name" == "dds" ]]; then
        subdir_name="opendds"
    fi

    # Make log dirs
    mkdir -p "${RUN_DIR}/out/${subdir_name}"
    chmod 777 "${RUN_DIR}/out/${subdir_name}"

    # Make results dirs
    mkdir -p "${RESULTS_ROOT}/${subdir_name}"
    chmod 777 "${RESULTS_ROOT}/${subdir_name}"
    

    for repeat in $(seq 1 "${REPEAT_COUNT}"); do
      marker=$(mktemp)
      touch "${marker}"

      run_tag="${scenario_name}_${broker}_run${repeat}"
      echo
      echo "=== ${broker}: ${scenario_name} (run ${repeat}/${REPEAT_COUNT}) ==="

      current_compose="${run_compose_file}"
      if ! (SCEN_DIR="${container_dir}" \
            SCENARIO="${scenario_name}" \
            RUN_TAG="${run_tag}" \
            docker compose -f "${run_compose_file}" up --force-recreate --abort-on-container-exit)
      then
        echo "Run failed for broker ${broker} and scenario ${scenario_name}" >&2
        rm -f "${marker}"
        exit 1
      fi

      docker compose -f "${run_compose_file}" down --remove-orphans >/dev/null
      current_compose=""

      move_new_entries "${RESULTS_ROOT}" "${RESULTS_ROOT}/${subdir_name}" "${marker}" "${RUN_DIR}/out/${subdir_name}"

      rm -f "${marker}"
    done
    
    # Clean up after
    rm -rf "${RUN_DIR}/out"
    rm $run_compose_file
    
  done
done
