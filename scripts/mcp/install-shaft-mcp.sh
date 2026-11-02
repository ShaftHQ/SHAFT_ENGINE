#!/usr/bin/env sh
# Deprecated shim. Delegates to install-shaft-agentic-tools.sh.
set -eu

fail() {
  echo "$1" >&2
  exit 1
}

echo "install-shaft-mcp.sh is deprecated; delegating to install-shaft-agentic-tools.sh" >&2

script_dir=""
case "$0" in
  */*)
    script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" 2>/dev/null && pwd -P || true)"
    ;;
esac

if [ -n "$script_dir" ] && [ -f "$script_dir/install-shaft-agentic-tools.sh" ]; then
  sh "$script_dir/install-shaft-agentic-tools.sh" "$@"
  exit $?
fi

if command -v curl >/dev/null 2>&1; then
  fetch() {
    curl -fsSL --retry 3 --retry-delay 2 -o "$1" "$2"
  }
elif command -v wget >/dev/null 2>&1; then
  fetch() {
    wget -q -O "$1" "$2"
  }
else
  fail "curl or wget is required to download the SHAFT Agentic Tools installer."
fi

repository="${SHAFT_MCP_REPOSITORY:-ShaftHQ/SHAFT_ENGINE}"
branch="${SHAFT_MCP_INSTALLER_REF:-main}"
url="https://raw.githubusercontent.com/${repository}/${branch}/scripts/mcp/install-shaft-agentic-tools.sh"
work="$(mktemp -d "${TMPDIR:-/tmp}/shaft-agentic-tools-shim-XXXXXX")"
cleanup() {
  rm -rf "$work"
}
trap cleanup EXIT INT TERM
script="$work/install-shaft-agentic-tools.sh"
fetch "$script" "$url"
chmod +x "$script"
sh "$script" "$@"
