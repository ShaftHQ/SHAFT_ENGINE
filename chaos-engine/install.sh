#!/usr/bin/env sh
# Install or upgrade ChaosEngine into the current directory.
# Run this from the target project folder:
#   export CHAOS_ENGINE_REPOSITORY=owner/repository
#   curl -fsSL "https://raw.githubusercontent.com/${CHAOS_ENGINE_REPOSITORY}/main/chaos-engine/install.sh" | bash
set -eu

fail() {
  echo "$1" >&2
  exit 1
}

if [ -z "${CHAOS_ENGINE_REPOSITORY:-}" ]; then
  fail "Set CHAOS_ENGINE_REPOSITORY to the upstream owner/repository before running this installer."
fi
repository="$CHAOS_ENGINE_REPOSITORY"
branch="${CHAOS_ENGINE_BRANCH:-main}"
project="$(pwd)"

if command -v python3 >/dev/null 2>&1; then
  python="python3"
elif command -v python >/dev/null 2>&1; then
  python="python"
else
  fail "Python 3 is required (python3 or python)."
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
  fail "curl or wget is required to download the ChaosEngine bootstrap."
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/chaos-engine-bootstrap-XXXXXX")"
cleanup() {
  rm -rf "$work"
}
trap cleanup EXIT INT TERM

bootstrap="$work/bootstrap.py"
url="https://raw.githubusercontent.com/${repository}/${branch}/chaos-engine/bootstrap.py"
echo "Installing ChaosEngine into ${project} from ${repository}@${branch}"
fetch "$bootstrap" "$url"
"$python" "$bootstrap" --project "$project" --repository "$repository" --branch "$branch"
