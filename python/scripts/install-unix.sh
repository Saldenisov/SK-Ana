#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo 'Usage: install-unix.sh macos|linux [--setup-only] [--dev] [--dry-run] [--port PORT]' >&2
  exit 2
fi
requested_platform=$1
shift
setup_only=0
dev=0
dry_run=0
port=8501
while [[ $# -gt 0 ]]; do
  case "$1" in
    --setup-only) setup_only=1; shift ;;
    --dev) dev=1; shift ;;
    --dry-run) dry_run=1; shift ;;
    --port)
      [[ $# -ge 2 && $2 =~ ^[0-9]+$ && $2 -ge 1 && $2 -le 65535 ]] || {
        echo '--port needs a number from 1 to 65535' >&2; exit 2;
      }
      port=$2; shift 2 ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
  esac
done

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
python_root=$(cd -- "$script_dir/.." && pwd -P)
case "$(uname -s)" in
  Darwin) actual_platform=macos ;;
  Linux) actual_platform=linux ;;
  *) echo 'Only macOS and Linux are supported by this script.' >&2; exit 2 ;;
esac
[[ $requested_platform == "$actual_platform" ]] || {
  echo "Use install-$actual_platform.sh on this system." >&2; exit 2;
}

case "$actual_platform:$(uname -m)" in
  macos:arm64|macos:aarch64)
    platform=MacOSX-arm64
    version=26.5.3-2
    sha256=ba7e4dede827e98c468c2dfa3b9a859728844ee8bde72a28eb216869af2d6ce1 ;;
  macos:x86_64)
    platform=MacOSX-x86_64
    version=25.7.0-2
    sha256=e8f6aed58d708cc544ba6bacbebad86787cb8df56667ff4729ad2fe36af32846 ;;
  linux:x86_64|linux:amd64)
    platform=Linux-x86_64
    version=26.5.3-2
    sha256=37606f9f03ced8ef60f4ffc76b21dda01728eac8a632dcab316c891cea4fe2f5 ;;
  linux:aarch64|linux:arm64)
    platform=Linux-aarch64
    version=26.5.3-2
    sha256=822923b30d789477964ef5589f86de5a72ac4c584bad3b9f72d97190e13d776f ;;
  *) echo "Unsupported architecture: $(uname -m)" >&2; exit 2 ;;
esac
installer_name="Miniconda3-py312_${version}-${platform}.sh"
installer_url="https://repo.anaconda.com/miniconda/$installer_name"
install_root=${SKANA_MINICONDA_ROOT:-"$HOME/.skana/miniconda3"}
environment_prefix=${SKANA_ENV_PREFIX:-"$HOME/.skana/envs/skana-python"}
package=$python_root
[[ $dev -eq 0 ]] || package="${package}[dev]"

if [[ $dry_run -eq 1 ]]; then
  printf 'Platform: %s\nMiniconda URL: %s\nSHA-256: %s\nEnvironment: %s\nPackage: %s\n' \
    "$platform" "$installer_url" "$sha256" "$environment_prefix" "$package"
  exit 0
fi

if [[ -n ${SKANA_CONDA_EXE:-} ]]; then
  conda_exe=$SKANA_CONDA_EXE
elif [[ -x "$install_root/bin/conda" ]]; then
  conda_exe="$install_root/bin/conda"
elif command -v conda >/dev/null 2>&1; then
  conda_exe=$(command -v conda)
else
  [[ ! -e $install_root ]] || {
    echo "Incomplete Miniconda installation at $install_root" >&2; exit 1;
  }
  command -v curl >/dev/null 2>&1 || { echo 'curl is required to download Miniconda.' >&2; exit 1; }
  mkdir -p -- "$(dirname -- "$install_root")"
  installer_path=$(mktemp "${TMPDIR:-/tmp}/skana-miniconda.XXXXXXXX.sh")
  trap 'rm -f -- "$installer_path"' EXIT
  echo "Downloading Miniconda from $installer_url"
  curl --fail --location --silent --show-error --proto '=https' --tlsv1.2 \
    "$installer_url" --output "$installer_path"
  if command -v shasum >/dev/null 2>&1; then
    actual_hash=$(shasum -a 256 "$installer_path" | awk '{print $1}')
  else
    actual_hash=$(sha256sum "$installer_path" | awk '{print $1}')
  fi
  [[ $actual_hash == "$sha256" ]] || {
    echo "Miniconda SHA-256 mismatch: $actual_hash" >&2; exit 1;
  }
  bash "$installer_path" -b -p "$install_root"
  rm -f -- "$installer_path"
  trap - EXIT
  conda_exe="$install_root/bin/conda"
fi

[[ -x $conda_exe ]] || { echo "Conda executable not found: $conda_exe" >&2; exit 1; }
if [[ ! -f "$environment_prefix/conda-meta/history" ]]; then
  "$conda_exe" create --yes --prefix "$environment_prefix" \
    --override-channels --channel conda-forge python=3.12 pip
fi
"$conda_exe" run --no-capture-output --prefix "$environment_prefix" \
  python -m pip install --disable-pip-version-check -e "$package"
"$conda_exe" run --no-capture-output --prefix "$environment_prefix" python -m pip check

echo "SK-Ana Python is installed in $environment_prefix"
if [[ $setup_only -eq 0 ]]; then
  echo "Open http://127.0.0.1:$port/"
  exec "$conda_exe" run --no-capture-output --prefix "$environment_prefix" \
    python -m streamlit run "$python_root/skana/app.py" \
    --server.address 127.0.0.1 --server.port "$port" --server.headless true
fi
