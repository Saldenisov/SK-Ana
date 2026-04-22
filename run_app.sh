#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ORIGINAL_CWD="$(pwd)"
REPO_URL="${SK_ANA_REPO_URL:-https://github.com/Saldenisov/SK-Ana.git}"
REPO_BRANCH="${SK_ANA_BRANCH:-master}"
BOOTSTRAP_DIR_NAME="${SK_ANA_BOOTSTRAP_DIR:-SK-Ana}"
MANAGED_CHECKOUT_DIR_NAME="${SK_ANA_MANAGED_CHECKOUT_DIR:-.sk_ana_checkout}"
LAUNCH_MODE="standalone"
SNAPSHOT_ROOT=""
REPO_ROOT=""

log_step() {
  printf '\n==> %s\n' "$1"
}

log_info() {
  printf '    %s\n' "$1"
}

looks_like_repo_root() {
  local path="$1"
  [[ -f "${path}/app.R" && -f "${path}/scripts/run_app_3840.sh" ]]
}

is_git_checkout() {
  local path="$1"
  [[ -d "${path}/.git" ]]
}

managed_checkout_root() {
  local source_root="$1"
  printf '%s\n' "${source_root}/${MANAGED_CHECKOUT_DIR_NAME}/${BOOTSTRAP_DIR_NAME}"
}

resolve_repo_root() {
  local candidate

  for candidate in \
    "${SCRIPT_DIR}" \
    "${ORIGINAL_CWD}" \
    "${SCRIPT_DIR}/${BOOTSTRAP_DIR_NAME}" \
    "${ORIGINAL_CWD}/${BOOTSTRAP_DIR_NAME}"
  do
    if looks_like_repo_root "${candidate}" && is_git_checkout "${candidate}"; then
      LAUNCH_MODE="git_checkout"
      REPO_ROOT="${candidate}"
      return 0
    fi
  done

  for candidate in \
    "${SCRIPT_DIR}" \
    "${ORIGINAL_CWD}" \
    "${SCRIPT_DIR}/${BOOTSTRAP_DIR_NAME}" \
    "${ORIGINAL_CWD}/${BOOTSTRAP_DIR_NAME}"
  do
    if looks_like_repo_root "${candidate}"; then
      LAUNCH_MODE="snapshot"
      SNAPSHOT_ROOT="${candidate}"
      REPO_ROOT="$(managed_checkout_root "${candidate}")"
      return 0
    fi
  done

  LAUNCH_MODE="standalone"
  REPO_ROOT="${SCRIPT_DIR}/${BOOTSTRAP_DIR_NAME}"
}

have_git() {
  command -v git >/dev/null 2>&1
}

repo_is_dirty() {
  local repo_root="$1"
  [[ -n "$(git -C "${repo_root}" status --porcelain 2>/dev/null)" ]]
}

update_repo_if_possible() {
  local repo_root="$1"

  if [[ ! -d "${repo_root}/.git" ]]; then
    return 0
  fi

  if ! have_git; then
    log_info "Git is not available. Using existing local checkout without updating."
    return 0
  fi

  if repo_is_dirty "${repo_root}"; then
    log_info "Local checkout has uncommitted changes. Skipping automatic git pull."
    return 0
  fi

  log_step "Updating SK-Ana checkout"
  git -C "${repo_root}" pull --ff-only origin "${REPO_BRANCH}"
}

download_repo_archive() {
  local repo_root="$1"
  local owner_repo archive_url tmpdir extracted_root

  owner_repo="$(printf '%s' "${REPO_URL}" | sed -E 's#^https://github.com/##; s#\.git$##')"
  archive_url="https://github.com/${owner_repo}/archive/refs/heads/${REPO_BRANCH}.tar.gz"

  log_step "Downloading SK-Ana source archive"
  log_info "Source: ${archive_url}"

  tmpdir="$(mktemp -d)"
  trap 'rm -rf "${tmpdir}"' RETURN

  curl -L "${archive_url}" -o "${tmpdir}/sk-ana.tar.gz"
  mkdir -p "${repo_root}"
  tar -xzf "${tmpdir}/sk-ana.tar.gz" -C "${tmpdir}"
  extracted_root="$(find "${tmpdir}" -mindepth 1 -maxdepth 1 -type d -name 'SK-Ana-*' | head -n 1)"

  if [[ -z "${extracted_root}" ]]; then
    echo "Failed to unpack SK-Ana archive." >&2
    return 1
  fi

  rm -rf "${repo_root}"
  mv "${extracted_root}" "${repo_root}"
  log_info "Downloaded SK-Ana to ${repo_root}"
}

clone_repo() {
  local repo_root="$1"

  log_step "Cloning SK-Ana from GitHub"
  git clone --depth 1 --branch "${REPO_BRANCH}" "${REPO_URL}" "${repo_root}"
}

bootstrap_repo_if_needed() {
  local repo_root="$1"

  if looks_like_repo_root "${repo_root}"; then
    return 0
  fi

  if [[ -e "${repo_root}" && ! -d "${repo_root}" ]]; then
    echo "Bootstrap target exists but is not a directory: ${repo_root}" >&2
    return 1
  fi

  if have_git; then
    clone_repo "${repo_root}"
    return 0
  fi

  log_info "Git is not installed. Falling back to a direct GitHub source download."
  download_repo_archive "${repo_root}"
}

main() {
  resolve_repo_root

  log_step "Preparing SK-Ana launcher"
  log_info "Script location: ${SCRIPT_DIR}"
  log_info "Target repo: ${REPO_ROOT}"
  case "${LAUNCH_MODE}" in
    git_checkout)
      log_info "Detected a git checkout. Using it directly."
      ;;
    snapshot)
      log_info "Detected a snapshot copy without .git."
      log_info "Bootstrapping a managed checkout in ${REPO_ROOT}"
      ;;
    *)
      log_info "Detected a standalone launcher."
      ;;
  esac

  if [[ "${LAUNCH_MODE}" != "git_checkout" ]]; then
    bootstrap_repo_if_needed "${REPO_ROOT}"
  fi
  update_repo_if_possible "${REPO_ROOT}"

  if [[ "${REPO_ROOT}" != "${SCRIPT_DIR}" ]]; then
    if [[ -x "${REPO_ROOT}/run_app.sh" ]]; then
      log_info "Handing off to ${REPO_ROOT}/run_app.sh"
      exec "${REPO_ROOT}/run_app.sh" "$@"
    fi

    log_info "Handing off directly to ${REPO_ROOT}/scripts/run_app_3840.sh"
    exec "${REPO_ROOT}/scripts/run_app_3840.sh" "$@"
  fi

  if [[ ! -x "${REPO_ROOT}/.R_skana/micromamba/envs/R_skana/bin/Rscript" ]]; then
    log_info "First launch detected. SK-Ana will install the isolated R_skana runtime and required packages before starting."
  fi

  exec "${REPO_ROOT}/scripts/run_app_3840.sh" "$@"
}

main "$@"
