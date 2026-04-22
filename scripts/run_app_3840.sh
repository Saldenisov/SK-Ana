#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

HOST="${SK_ANA_HOST:-127.0.0.1}"
PORT="${PORT:-3840}"

cd "${REPO_ROOT}"

echo "Preparing SK-Ana launch from isolated R_skana environment (preferred http://${HOST}:${PORT})"
exec "${SCRIPT_DIR}/r_skana.sh" Rscript "${SCRIPT_DIR}/run_app_3840.R"
