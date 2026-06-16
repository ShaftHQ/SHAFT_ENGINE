#!/usr/bin/env sh
set -eu

PYTHON_RELEASE="20260610"
PYTHON_VERSION="3.13.14"

fail() {
  echo "install-shaft-mcp: $1" >&2
  exit "${2:-1}"
}

download() {
  url="$1"
  output="$2"
  mkdir -p "$(dirname "$output")"
  if command -v curl >/dev/null 2>&1; then
    curl -fL --retry 3 -o "$output" "$url"
  elif command -v wget >/dev/null 2>&1; then
    wget -O "$output" "$url"
  else
    fail "curl or wget is required to download installer dependencies." 3
  fi
}

sha256_file() {
  path="$1"
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$path" | awk '{print tolower($1)}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$path" | awk '{print tolower($1)}'
  elif command -v openssl >/dev/null 2>&1; then
    openssl dgst -sha256 "$path" | awk '{print tolower($NF)}'
  else
    fail "sha256sum, shasum, or openssl is required for checksum verification." 3
  fi
}

bootstrap_root() {
  if [ -n "${SHAFT_MCP_BOOTSTRAP_HOME:-}" ]; then
    printf '%s\n' "$SHAFT_MCP_BOOTSTRAP_HOME"
    return 0
  fi
  system_name="$(uname -s)"
  case "$system_name" in
    Darwin)
      printf '%s\n' "$HOME/Library/Caches/ShaftHQ/shaft-mcp-bootstrap"
      ;;
    Linux)
      printf '%s\n' "${XDG_CACHE_HOME:-$HOME/.cache}/shafthq/shaft-mcp-bootstrap"
      ;;
    *)
      fail "unsupported operating system: $system_name" 3
      ;;
  esac
}

test_python() {
  "$1" -c 'import sys; raise SystemExit(0 if sys.version_info >= (3, 9) else 1)' >/dev/null 2>&1
}

find_python() {
  if command -v python3 >/dev/null 2>&1 && test_python "$(command -v python3)"; then
    command -v python3
    return 0
  fi
  if command -v python >/dev/null 2>&1 && test_python "$(command -v python)"; then
    command -v python
    return 0
  fi
  return 1
}

python_asset() {
  system_name="$(uname -s)"
  machine_name="$(uname -m)"
  case "$machine_name" in
    x86_64|amd64)
      cpu="x86_64"
      ;;
    arm64|aarch64)
      cpu="aarch64"
      ;;
    *)
      fail "unsupported architecture: $machine_name" 3
      ;;
  esac
  case "$system_name" in
    Darwin)
      os_name="apple-darwin"
      ;;
    Linux)
      os_name="unknown-linux-gnu"
      ;;
    *)
      fail "unsupported operating system: $system_name" 3
      ;;
  esac
  target="$cpu-$os_name"
  case "$target" in
    x86_64-apple-darwin)
      sha="592d3d807a493e4e21dbc972f81d1b2ece6381a7e687bcf0da68555a1282d49a"
      ;;
    aarch64-apple-darwin)
      sha="0e255968ed96255df59b6bc9504545260c11de3171e48f7640668d88154945ba"
      ;;
    x86_64-unknown-linux-gnu)
      sha="31ebbb024fecb446d7f25a5b59ca1d4955597ca3f5b4b76f55b30a8987eb61c9"
      ;;
    aarch64-unknown-linux-gnu)
      sha="fe5b366ecf26fe565113b195a03ea18d7030f2dab691c0bed41cf19318100be2"
      ;;
    *)
      fail "unsupported Python target: $target" 3
      ;;
  esac
  url="https://github.com/astral-sh/python-build-standalone/releases/download/$PYTHON_RELEASE/cpython-$PYTHON_VERSION%2B$PYTHON_RELEASE-$target-install_only.tar.gz"
  printf '%s|%s|%s\n' "$target" "$url" "$sha"
}

find_portable_python() {
  install_root="$1"
  [ -d "$install_root" ] || return 1
  candidates="$install_root/.python-candidates"
  find "$install_root" -type f \( -name python3 -o -name python \) > "$candidates" 2>/dev/null || true
  while IFS= read -r candidate; do
    if test_python "$candidate"; then
      printf '%s\n' "$candidate"
      rm -f "$candidates"
      return 0
    fi
  done < "$candidates"
  rm -f "$candidates"
  return 1
}

install_portable_python() {
  root="$1"
  asset="$(python_asset)"
  target="$(printf '%s' "$asset" | cut -d'|' -f1)"
  url="$(printf '%s' "$asset" | cut -d'|' -f2)"
  expected="$(printf '%s' "$asset" | cut -d'|' -f3)"
  install_root="$root/tools/python/$PYTHON_VERSION-$PYTHON_RELEASE-$target"
  if python_path="$(find_portable_python "$install_root")"; then
    printf '%s\n' "$python_path"
    return 0
  fi
  command -v tar >/dev/null 2>&1 || fail "Python is unavailable and tar is required to extract the portable Python runtime." 3
  archive="$root/downloads/python-$PYTHON_VERSION-$PYTHON_RELEASE-$target.tar.gz"
  echo "Downloading portable Python $PYTHON_VERSION for $target..." >&2
  download "$url" "$archive"
  actual="$(sha256_file "$archive")"
  [ "$actual" = "$expected" ] || fail "Checksum verification failed for the portable Python runtime." 3
  temporary="$root/tools/python/.extract-$$"
  rm -rf "$temporary" "$install_root"
  mkdir -p "$temporary" "$(dirname "$install_root")"
  tar -xzf "$archive" -C "$temporary"
  mv "$temporary" "$install_root"
  python_path="$(find_portable_python "$install_root")" || fail "Portable Python archive did not contain a usable Python executable." 3
  printf '%s\n' "$python_path"
}

resolve_python_script() {
  root="$1"
  script_dir=""
  case "$0" in
    */*)
      script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" 2>/dev/null && pwd -P || true)"
      ;;
  esac
  if [ -n "$script_dir" ] && [ -f "$script_dir/install_shaft_mcp.py" ]; then
    printf '%s\n' "$script_dir/install_shaft_mcp.py"
    return 0
  fi
  ref="${SHAFT_MCP_INSTALLER_REF:-main}"
  url="${SHAFT_MCP_INSTALLER_PYTHON_URL:-https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/$ref/scripts/mcp/install_shaft_mcp.py}"
  target="$root/scripts/install_shaft_mcp.py"
  download "$url" "$target"
  printf '%s\n' "$target"
}

ROOT="$(bootstrap_root)"
mkdir -p "$ROOT"

if python_path="$(find_python)"; then
  :
else
  python_path="$(install_portable_python "$ROOT")"
fi

python_script="$(resolve_python_script "$ROOT")"
exec "$python_path" "$python_script" "$@"
