#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ORIGINAL_CWD="$(pwd)"
REPO_URL="${SK_ANA_REPO_URL:-https://github.com/Saldenisov/SK-Ana.git}"
REPO_BRANCH="${SK_ANA_BRANCH:-master}"
BOOTSTRAP_DIR_NAME="${SK_ANA_BOOTSTRAP_DIR:-SK-Ana}"
LAUNCH_MODE="standalone"
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
      REPO_ROOT="${candidate}"
      return 0
    fi
  done

  LAUNCH_MODE="standalone"
  REPO_ROOT="${SCRIPT_DIR}/${BOOTSTRAP_DIR_NAME}"
}

have_git() {
  command -v git >/dev/null 2>&1
}

run_with_optional_sudo() {
  if command -v sudo >/dev/null 2>&1 && [[ "${EUID:-$(id -u)}" -ne 0 ]]; then
    sudo "$@"
    return $?
  fi

  "$@"
}

install_git() {
  if have_git; then
    return 0
  fi

  log_step "Installing Git"

  if command -v brew >/dev/null 2>&1; then
    brew install git
    return $?
  fi

  if command -v apt-get >/dev/null 2>&1; then
    run_with_optional_sudo apt-get update
    run_with_optional_sudo apt-get install -y git
    return $?
  fi

  if command -v dnf >/dev/null 2>&1; then
    run_with_optional_sudo dnf install -y git
    return $?
  fi

  if command -v yum >/dev/null 2>&1; then
    run_with_optional_sudo yum install -y git
    return $?
  fi

  if command -v pacman >/dev/null 2>&1; then
    run_with_optional_sudo pacman -Sy --noconfirm git
    return $?
  fi

  if command -v zypper >/dev/null 2>&1; then
    run_with_optional_sudo zypper --non-interactive install git
    return $?
  fi

  if command -v apk >/dev/null 2>&1; then
    run_with_optional_sudo apk add --no-cache git
    return $?
  fi

  echo "Git is required but no supported installer was found." >&2
  return 1
}

ensure_git() {
  if have_git; then
    return 0
  fi

  install_git
  have_git
}

sync_repo_to_remote() {
  local repo_root="$1"

  log_step "Synchronizing SK-Ana checkout"
  log_info "Discarding local tracked changes and resetting to origin/${REPO_BRANCH}"

  git -C "${repo_root}" fetch --depth 1 origin "${REPO_BRANCH}"
  git -C "${repo_root}" reset --hard "origin/${REPO_BRANCH}"
  git -C "${repo_root}" clean -fd
}

initialize_snapshot_repo() {
  local repo_root="$1"

  ensure_git
  if ! have_git; then
    echo "Git is required to initialize the local snapshot checkout, but installation failed." >&2
    return 1
  fi

  log_step "Initializing git checkout in the extracted SK-Ana folder"
  git -C "${repo_root}" init
  git -C "${repo_root}" remote remove origin >/dev/null 2>&1 || true
  git -C "${repo_root}" remote add origin "${REPO_URL}"

  sync_repo_to_remote "${repo_root}"
}

update_repo_if_possible() {
  local repo_root="$1"

  if [[ ! -d "${repo_root}/.git" ]]; then
    return 0
  fi

  ensure_git
  if ! have_git; then
    echo "Git is required to update the local checkout, but installation failed." >&2
    return 1
  fi

  sync_repo_to_remote "${repo_root}"
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

  if looks_like_repo_root "${repo_root}"; then
    initialize_snapshot_repo "${repo_root}"
    return $?
  fi

  ensure_git
  if ! have_git; then
    echo "Git is required to bootstrap the repository, but installation failed." >&2
    return 1
  fi

  clone_repo "${repo_root}"
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
      log_info "Initializing git in this extracted folder and synchronizing from origin/${REPO_BRANCH}"
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
