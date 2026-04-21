#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

HOST="${HOST:-127.0.0.1}"
PORT="${PORT:-3840}"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found in PATH." >&2
  exit 127
fi

cd "${REPO_ROOT}"

echo "Launching SK-Ana from ${REPO_ROOT} on http://${HOST}:${PORT}"
exec Rscript -e "shiny::runApp('.', host='${HOST}', port=${PORT}, launch.browser=TRUE)"
