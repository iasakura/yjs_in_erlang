#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Display usage information
usage() {
  echo "Usage: $0 <headless_service_domain> <ip_prefix> -- <command> [args...]"
  exit 1
}

# Ensure at least four arguments are provided
if [ $# -lt 4 ]; then
  usage
fi

# Parse the first two arguments: headless service domain and IP prefix
SERVICE_DOMAIN="$1"
IP_PREFIX="$2"
shift 2

# The next argument must be '--' as a separator
if [ "$1" != "--" ]; then
  usage
fi
shift

# The remaining arguments form the command to execute
COMMAND=("$@")

# If YJS_IN_ERLANG_NODES is not set externally, perform DNS lookup (Kubernetes mode)
if [ -z "${YJS_IN_ERLANG_NODES:-}" ]; then
  # Ensure the 'dig' command is available
  if ! command -v dig &> /dev/null; then
    echo "Error: 'dig' command not found. Please install dnsutils or bind-tools."
    exit 1
  fi

  # Attempt to retrieve IP addresses from the headless service domain with retries
  MAX_ATTEMPTS=10
  SLEEP_TIME=2
  attempt=1
  while true; do
    IPS=$(dig +short "$SERVICE_DOMAIN")
    if [ -n "$IPS" ]; then
      break
    fi
    if [ "$attempt" -ge "$MAX_ATTEMPTS" ]; then
      echo "Error: Unable to retrieve IP addresses for $SERVICE_DOMAIN after ${attempt} attempts."
      exit 1
    fi
    echo "Attempt ${attempt}: No IP addresses found. Retrying in ${SLEEP_TIME} seconds..."
    sleep "$SLEEP_TIME"
    attempt=$((attempt + 1))
  done

  # Read the retrieved IP addresses into an array
  mapfile -t ip_array <<< "$IPS"

  # Prepend the specified IP prefix to each IP address
  prefixed_ips=()
  for ip in "${ip_array[@]}"; do
    prefixed_ips+=("${IP_PREFIX}${ip}")
  done

  # Join the prefixed IP addresses into a space-separated string
  YJS_IN_ERLANG_NODES=$(IFS=' '; echo "${prefixed_ips[*]}")
  export YJS_IN_ERLANG_NODES
  echo "Retrieved YJS_IN_ERLANG_NODES: $YJS_IN_ERLANG_NODES"
else
  # Use the externally provided YJS_IN_ERLANG_NODES (docker-compose mode)
  echo "Using externally provided YJS_IN_ERLANG_NODES: $YJS_IN_ERLANG_NODES"
fi

# Execute the specified command, replacing the current shell process
exec "${COMMAND[@]}"
