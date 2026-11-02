#!/usr/bin/env sh
# Install SHAFT Agentic Tools (MCP, CLI, and skills) into the current directory.
# Change into the target project first:
#   curl -fsSL "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install.sh" | bash
set -eu

fail() {
  echo "$1" >&2
  exit 1
}

repository="${SHAFT_MCP_REPOSITORY:-ShaftHQ/SHAFT_ENGINE}"
branch="${SHAFT_MCP_INSTALLER_REF:-main}"
url="https://raw.githubusercontent.com/${repository}/${branch}/scripts/mcp/install-shaft-agentic-tools.sh"

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

work="$(mktemp -d "${TMPDIR:-/tmp}/shaft-agentic-tools-one-liner-XXXXXX")"
cleanup() {
  rm -rf "$work"
}
trap cleanup EXIT INT TERM

script="$work/install-shaft-agentic-tools.sh"
echo "Installing SHAFT Agentic Tools into $(pwd) from ${repository}@${branch}"
fetch "$script" "$url"
chmod +x "$script"
sh "$script" "$@"
