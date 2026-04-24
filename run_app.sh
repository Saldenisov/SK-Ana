#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ORIGINAL_CWD="$(pwd)"
REPO_URL="${SK_ANA_REPO_URL:-https://github.com/Saldenisov/SK-Ana.git}"
REPO_BRANCH="${SK_ANA_BRANCH:-master}"
BOOTSTRAP_DIR_NAME="${SK_ANA_BOOTSTRAP_DIR:-SK-Ana}"
LAUNCH_MODE="standalone"
REPO_ROOT=""
LAUNCH_LOCK_DIR="${SK_ANA_LOCK_DIR:-}"
LOCK_OWNED="${SK_ANA_LOCK_OWNER:-0}"
DEFAULT_PORT="${PORT:-3840}"

log_step() {
  printf '\n==> %s\n' "$1"
}

log_info() {
  printf '    %s\n' "$1"
}

release_launch_lock() {
  if [[ "${LOCK_OWNED}" == "1" && -n "${LAUNCH_LOCK_DIR}" && -d "${LAUNCH_LOCK_DIR}" ]]; then
    rmdir "${LAUNCH_LOCK_DIR}" >/dev/null 2>&1 || true
  fi
}

acquire_launch_lock() {
  local lock_root

  lock_root="${REPO_ROOT}/.R_skana"
  mkdir -p "${lock_root}"
  LAUNCH_LOCK_DIR="${lock_root}/run_app.lock"

  if mkdir "${LAUNCH_LOCK_DIR}" >/dev/null 2>&1; then
    LOCK_OWNED="1"
    trap release_launch_lock EXIT
    return 0
  fi

  printf '\n============================================================\n' >&2
  printf 'SK-ANA IS ALREADY RUNNING IN ANOTHER TERMINAL WINDOW.\n' >&2
  printf 'CLOSE THE PREVIOUS SK-ANA TERMINAL, THEN RUN THIS AGAIN.\n' >&2
  printf 'If you are sure no SK-Ana launcher is running, delete:\n' >&2
  printf '  %s\n' "${LAUNCH_LOCK_DIR}" >&2
  printf '============================================================\n' >&2
  return 11
}

is_stop_command() {
  case "${1:-}" in
    stop|--stop)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

list_listener_pids() {
  local port="${1:-${DEFAULT_PORT}}"

  if command -v lsof >/dev/null 2>&1; then
    lsof -nP -iTCP:"${port}" -sTCP:LISTEN -t 2>/dev/null | awk '!seen[$0]++'
  fi
}

list_sk_ana_process_pids() {
  if command -v pgrep >/dev/null 2>&1; then
    pgrep -af 'run_app_3840\.R' 2>/dev/null | awk -v repo_root="${REPO_ROOT}" '
      index($0, repo_root) > 0 { print $1 }
    ' | awk '!seen[$0]++'
  fi
}

pid_is_running() {
  local pid="$1"
  kill -0 "${pid}" >/dev/null 2>&1
}

terminate_pid() {
  local pid="$1"

  if [[ -z "${pid}" ]] || ! pid_is_running "${pid}"; then
    return 0
  fi

  log_info "Stopping PID ${pid}"
  kill -TERM "${pid}" >/dev/null 2>&1 || true
  for _ in 1 2 3 4 5; do
    if ! pid_is_running "${pid}"; then
      return 0
    fi
    sleep 0.2
  done

  kill -KILL "${pid}" >/dev/null 2>&1 || true
}

stop_sk_ana() {
  local port="${PORT:-${DEFAULT_PORT}}"
  local lock_dir
  local stopped_any=0
  local pid
  local -a pids=()

  resolve_repo_root
  lock_dir="${REPO_ROOT}/.R_skana/run_app.lock"

  while IFS= read -r pid; do
    [[ -n "${pid}" ]] && pids+=("${pid}")
  done < <(list_listener_pids "${port}")

  if [[ "${#pids[@]}" -eq 0 ]]; then
    while IFS= read -r pid; do
      [[ -n "${pid}" ]] && pids+=("${pid}")
    done < <(list_sk_ana_process_pids)
  fi

  if [[ "${#pids[@]}" -eq 0 ]]; then
    if [[ -d "${lock_dir}" ]]; then
      rm -rf "${lock_dir}"
      log_step "Stopping SK-Ana"
      log_info "No running SK-Ana process was found. Removed stale lock ${lock_dir}."
      return 0
    fi

    log_step "Stopping SK-Ana"
    log_info "No running SK-Ana process was found on port ${port}."
    return 0
  fi

  log_step "Stopping SK-Ana"
  for pid in "${pids[@]}"; do
    if pid_is_running "${pid}"; then
      terminate_pid "${pid}"
      stopped_any=1
    fi
  done

  if [[ -d "${lock_dir}" ]]; then
    rm -rf "${lock_dir}"
    log_info "Removed launch lock ${lock_dir}."
  fi

  if [[ "${stopped_any}" == "1" ]]; then
    log_info "Stopped SK-Ana listener(s) on port ${port}."
  else
    log_info "No running SK-Ana process remained after the stop attempt."
  fi
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

repo_is_dirty() {
  local repo_root="$1"

  [[ -n "$(git -C "${repo_root}" status --porcelain --untracked-files=normal 2>/dev/null)" ]]
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

  if [[ "${SK_ANA_FORCE_SYNC:-0}" != "1" ]] && repo_is_dirty "${repo_root}"; then
    log_step "Skipping checkout synchronization"
    log_info "Local changes detected; preserving the current checkout."
    log_info "Set SK_ANA_FORCE_SYNC=1 to discard local changes and resync from origin/${REPO_BRANCH}."
    return 0
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

  if is_stop_command "${1:-}"; then
    stop_sk_ana
    return 0
  fi

  if [[ "${SK_ANA_LOCK_HELD:-0}" == "1" ]]; then
    trap release_launch_lock EXIT
  else
    acquire_launch_lock || return $?
    export SK_ANA_LOCK_HELD=1
    export SK_ANA_LOCK_DIR="${LAUNCH_LOCK_DIR}"
    export SK_ANA_LOCK_OWNER="${LOCK_OWNED}"
  fi

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
