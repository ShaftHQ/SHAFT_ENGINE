#!/usr/bin/env sh
# Install or upgrade ChaosEngine into the current directory.
# Run this from the target project folder:
#   curl -fsSL "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.sh" | bash -s -- "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.sh"
set -eu

fail() {
  echo "$1" >&2
  exit 1
}

valid_repository() {
  lowered=$(printf '%s\n' "$1" | tr '[:upper:]' '[:lower:]')
  case "$lowered" in
    ""|owner/repository) return 1 ;;
    */*) ;;
    *) return 1 ;;
  esac
  printf '%s\n' "$1" | grep -Eq '^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$'
}

is_chaos_engine_source_tree() {
  script_dir="$1"
  [ -n "$script_dir" ] || return 1
  [ -f "$script_dir/skills/chaos-engine/SKILL.md" ]
}

parse_chaos_engine_raw_url() {
  rest=$(printf '%s\n' "$1" | sed -n 's#.*https://raw.githubusercontent.com/\([A-Za-z0-9_.-]*\)/\([A-Za-z0-9_.-]*\)/\(.*\)/install\.sh.*#\1/\2|\3#p' | head -n 1)
  if [ -z "$rest" ]; then
    rest=$(printf '%s\n' "$1" | sed -n 's#.*https://raw.githubusercontent.com/\([A-Za-z0-9_.-]*\)/\([A-Za-z0-9_.-]*\)/\(.*\)/install\.ps1.*#\1/\2|\3#p' | head -n 1)
  fi
  [ -n "$rest" ] || return 1
  repository=${rest%%|*}
  path=${rest#*|}
  valid_repository "$repository" || return 1
  case "$path" in
    refs/heads/*|refs/tags/*)
      ref=$(printf '%s\n' "$path" | cut -d/ -f1-3)
      prefix=$(printf '%s\n' "$path" | cut -d/ -f4-)
      ;;
    *)
      ref=${path%%/*}
      if [ "$path" = "$ref" ]; then
        prefix=
      else
        prefix=${path#*/}
      fi
      ;;
  esac
  if [ -n "$prefix" ]; then
    bootstrap_path="$prefix/bootstrap.py"
  else
    bootstrap_path="bootstrap.py"
  fi
  printf '%s|%s|%s|%s\n' "$repository" "$ref" "$prefix" "https://raw.githubusercontent.com/${repository}/${ref}/${bootstrap_path}"
}

collect_source_text() {
  printf '%s\n' "$1"
  if [ -n "${CHAOS_ENGINE_INSTALL_URL:-}" ]; then
    printf '%s\n' "$CHAOS_ENGINE_INSTALL_URL"
  fi
  if command -v ps >/dev/null 2>&1 && [ -n "${PPID:-}" ]; then
    ps -o args= -p "$PPID" 2>/dev/null || true
  fi
}

resolve_source() {
  parsed=
  while IFS= read -r line; do
    [ -n "$line" ] || continue
    parsed=$(parse_chaos_engine_raw_url "$line" || true)
    if [ -n "$parsed" ]; then
      printf '%s\n' "$parsed"
      return 0
    fi
  done <<EOF
$(collect_source_text "${1:-}")
EOF
  env_repository=${CHAOS_ENGINE_REPOSITORY:-}
  if valid_repository "$env_repository"; then
    ref=${CHAOS_ENGINE_BRANCH:-main}
    printf '%s|%s|%s|%s\n' "$env_repository" "$ref" "chaos-engine" "https://raw.githubusercontent.com/${env_repository}/${ref}/chaos-engine/bootstrap.py"
    return 0
  fi
  fail "Put owner/repository in the install URL (or set CHAOS_ENGINE_REPOSITORY for a local file run)."
}

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

source_record=$(resolve_source "${1:-}")
repository=$(printf '%s\n' "$source_record" | cut -d'|' -f1)
ref=$(printf '%s\n' "$source_record" | cut -d'|' -f2)
prefix=$(printf '%s\n' "$source_record" | cut -d'|' -f3)
bootstrap_url=$(printf '%s\n' "$source_record" | cut -d'|' -f4-)
branch=${CHAOS_ENGINE_BRANCH:-$ref}
if [ -n "${CHAOS_ENGINE_BRANCH:-}" ]; then
  if [ -n "$prefix" ]; then
    bootstrap_url="https://raw.githubusercontent.com/${repository}/${branch}/${prefix}/bootstrap.py"
  else
    bootstrap_url="https://raw.githubusercontent.com/${repository}/${branch}/bootstrap.py"
  fi
fi
project="$(pwd)"
if [ "${CHAOS_ENGINE_RESOLVE_ONLY:-}" = 1 ]; then
  printf '%s|%s|%s|%s\n' "$repository" "$branch" "$prefix" "$bootstrap_url"
  exit 0
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/chaos-engine-bootstrap-XXXXXX")"
cleanup() {
  rm -rf "$work"
}
trap cleanup EXIT INT TERM

bootstrap="$work/bootstrap.py"
echo "Installing ChaosEngine into ${project} from ${repository}@${branch}"
case "$0" in
  */install.sh|install.sh)
    script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
    if [ -f "$script_dir/bootstrap.py" ] && is_chaos_engine_source_tree "$script_dir"; then
      cp "$script_dir/bootstrap.py" "$bootstrap"
    else
      fetch "$bootstrap" "$bootstrap_url"
    fi
    ;;
  *)
    fetch "$bootstrap" "$bootstrap_url"
    ;;
esac
"$python" "$bootstrap" --project "$project" --repository "$repository" --branch "$branch"
