#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
R_SKANA_DIR="${REPO_ROOT}/.R_skana"
TOOLS_DIR="${R_SKANA_DIR}/tools"
MICROMAMBA_DIR="${TOOLS_DIR}/micromamba"
MICROMAMBA_BIN="${MICROMAMBA_DIR}/bin/micromamba"
MAMBA_ROOT_PREFIX="${R_SKANA_DIR}/micromamba"
ENV_FILE="${SCRIPT_DIR}/r_skana.environment.yml"
ENV_RSCRIPT="${MAMBA_ROOT_PREFIX}/envs/R_skana/bin/Rscript"
ENV_STAMP_FILE="${R_SKANA_DIR}/base-environment.stamp"

mkdir -p "${TOOLS_DIR}"

log_step() {
  printf '\n==> %s\n' "$1"
}

log_info() {
  printf '    %s\n' "$1"
}

hash_file() {
  local file="$1"

  if command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "${file}" | awk '{print $1}'
    return 0
  fi

  if command -v md5sum >/dev/null 2>&1; then
    md5sum "${file}" | awk '{print $1}'
    return 0
  fi

  if command -v openssl >/dev/null 2>&1; then
    openssl dgst -sha256 "${file}" | awk '{print $NF}'
    return 0
  fi

  return 1
}

env_definition_changed() {
  local current_hash existing_hash

  if [[ ! -f "${ENV_STAMP_FILE}" ]]; then
    return 0
  fi

  current_hash="$(hash_file "${ENV_FILE}")" || return 0
  existing_hash="$(tr -d '\r\n' < "${ENV_STAMP_FILE}")"
  [[ "${current_hash}" != "${existing_hash}" ]]
}

write_env_stamp() {
  local current_hash
  current_hash="$(hash_file "${ENV_FILE}")" || return 0
  printf '%s\n' "${current_hash}" > "${ENV_STAMP_FILE}"
}

detect_micromamba_url() {
  local os arch platform
  os="$(uname -s)"
  arch="$(uname -m)"

  case "${os}" in
    Darwin)
      case "${arch}" in
        arm64|aarch64) platform="osx-arm64" ;;
        x86_64) platform="osx-64" ;;
        *) echo "Unsupported macOS architecture: ${arch}" >&2; return 1 ;;
      esac
      ;;
    Linux)
      case "${arch}" in
        x86_64) platform="linux-64" ;;
        arm64|aarch64) platform="linux-aarch64" ;;
        ppc64le) platform="linux-ppc64le" ;;
        *) echo "Unsupported Linux architecture: ${arch}" >&2; return 1 ;;
      esac
      ;;
    *)
      echo "Unsupported operating system: ${os}" >&2
      return 1
      ;;
  esac

  printf 'https://micro.mamba.pm/api/micromamba/%s/latest\n' "${platform}"
}

ensure_micromamba() {
  if [[ -x "${MICROMAMBA_BIN}" ]]; then
    log_info "Using existing micromamba: ${MICROMAMBA_BIN}"
    return 0
  fi

  local url tmpdir
  log_step "Downloading micromamba"
  url="$(detect_micromamba_url)"
  log_info "Source: ${url}"
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "${tmpdir}"' RETURN

  mkdir -p "${MICROMAMBA_DIR}"
  (
    cd "${tmpdir}"
    curl -L "${url}" | tar -xj
    mkdir -p "${MICROMAMBA_DIR}/bin"
    mv bin/micromamba "${MICROMAMBA_BIN}"
  )
  chmod +x "${MICROMAMBA_BIN}"
  log_info "Installed micromamba to ${MICROMAMBA_BIN}"
}

ensure_base_env() {
  if [[ -x "${ENV_RSCRIPT}" ]]; then
    log_step "Updating existing R_skana environment"
    "${MICROMAMBA_BIN}" env update -y -r "${MAMBA_ROOT_PREFIX}" -n R_skana \
      --override-channels -c conda-forge -f "${ENV_FILE}" --prune
    write_env_stamp
    return 0
  fi

  log_step "Creating R_skana environment"
  "${MICROMAMBA_BIN}" create -y -r "${MAMBA_ROOT_PREFIX}" \
    --override-channels -c conda-forge -f "${ENV_FILE}"
  write_env_stamp
}

check_existing_env() {
  if [[ ! -x "${ENV_RSCRIPT}" ]]; then
    return 1
  fi

  if env_definition_changed; then
    log_info "Base environment definition changed. Refreshing the R_skana environment."
    return 1
  fi

  log_step "Checking existing R_skana environment"
  if (
    cd "${REPO_ROOT}"
    "${MICROMAMBA_BIN}" run -r "${MAMBA_ROOT_PREFIX}" -n R_skana \
      Rscript "${SCRIPT_DIR}/setup_r_skana.R"
  ); then
    return 0
  fi

  log_info "Existing environment check failed. Refreshing the base environment."
  return 1
}

ensure_project_packages() {
  log_step "Installing SK-Ana R packages"
  (
    cd "${REPO_ROOT}"
    "${MICROMAMBA_BIN}" run -r "${MAMBA_ROOT_PREFIX}" -n R_skana \
      Rscript "${SCRIPT_DIR}/setup_r_skana.R"
  )
}

log_step "Preparing isolated R_skana runtime"
log_info "Repository root: ${REPO_ROOT}"
ensure_micromamba
if check_existing_env; then
  log_step "R_skana setup complete"
  exit 0
fi
ensure_base_env
ensure_project_packages
log_step "R_skana setup complete"
