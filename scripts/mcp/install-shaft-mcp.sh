#!/usr/bin/env sh
set -eu

MAVEN_VERSION="${SHAFT_MCP_BOOTSTRAP_MAVEN_VERSION:-3.9.12}"
DEPENDENCY_GOAL="org.apache.maven.plugins:maven-dependency-plugin:3.9.0:copy"
ARTIFACT="io.github.shafthq:shaft-mcp"
VERSION="${SHAFT_MCP_VERSION:-LATEST}"

usage() {
  cat >&2 <<'EOF'
Usage: install-shaft-mcp.sh <target>

Targets:
  --codex
  --codex-app
  --claude
  --claude-desktop
  --copilot
  --copilot-vscode
EOF
}

fail() {
  echo "install-shaft-mcp: $1" >&2
  exit "${2:-1}"
}

target="${1:-}"
case "$target" in
  -h|--help)
    usage
    exit 0
    ;;
  codex|codex-app|claude|claude-desktop|copilot|copilot-vscode)
    target="--$target"
    ;;
  --codex|--codex-app|--claude|--claude-desktop|--copilot|--copilot-vscode)
    ;;
  *)
    usage
    exit 2
    ;;
esac

if [ "$#" -ne 1 ]; then
  usage
  exit 2
fi

system_name="$(uname -s)"
machine_name="$(uname -m)"
case "$system_name" in
  Linux)
    adoptium_os="linux"
    cache_base="${XDG_CACHE_HOME:-$HOME/.cache}/shafthq/shaft-mcp-bootstrap"
    ;;
  Darwin)
    adoptium_os="mac"
    cache_base="$HOME/Library/Caches/ShaftHQ/shaft-mcp-bootstrap"
    ;;
  *)
    fail "unsupported operating system: $system_name" 3
    ;;
esac

case "$machine_name" in
  x86_64|amd64)
    adoptium_arch="x64"
    ;;
  arm64|aarch64)
    adoptium_arch="aarch64"
    ;;
  *)
    fail "unsupported architecture: $machine_name" 3
    ;;
esac

BOOTSTRAP_ROOT="${SHAFT_MCP_BOOTSTRAP_HOME:-$cache_base}"
DOWNLOAD_DIR="$BOOTSTRAP_ROOT/downloads"
TOOLS_DIR="$BOOTSTRAP_ROOT/tools"
WORK_DIR="$BOOTSTRAP_ROOT/work"
mkdir -p "$DOWNLOAD_DIR" "$TOOLS_DIR" "$WORK_DIR"

download() {
  url="$1"
  output="$2"
  if command -v curl >/dev/null 2>&1; then
    curl -fL --retry 3 -o "$output" "$url"
  elif command -v wget >/dev/null 2>&1; then
    wget -O "$output" "$url"
  else
    fail "curl or wget is required to download bootstrap dependencies." 3
  fi
}

java_feature() {
  output="$("$1" -version 2>&1)" || return 1
  raw="$(printf '%s\n' "$output" | sed -n 's/.*version "\([^"]*\)".*/\1/p' | head -n 1)"
  if [ -z "$raw" ]; then
    raw="$(printf '%s\n' "$output" | sed -n 's/.*openjdk \([0-9][^[:space:]]*\).*/\1/p' | head -n 1)"
  fi
  if [ -z "$raw" ]; then
    return 1
  fi
  case "$raw" in
    1.*)
      printf '%s' "$raw" | cut -d. -f2
      ;;
    *)
      printf '%s' "$raw" | sed 's/[._-].*$//'
      ;;
  esac
}

is_java25() {
  [ "$(java_feature "$1" 2>/dev/null || true)" = "25" ]
}

find_java_in() {
  search_root="$1"
  candidates="$BOOTSTRAP_ROOT/.java-candidates"
  : > "$candidates"
  if [ -d "$search_root" ]; then
    find "$search_root" -type f -path '*/bin/java' > "$candidates" 2>/dev/null || true
  fi
  while IFS= read -r candidate; do
    if is_java25 "$candidate"; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done < "$candidates"
  return 1
}

download_java25() {
  java_root="$TOOLS_DIR/jdk/temurin-25-$adoptium_os-$adoptium_arch"
  if java_path="$(find_java_in "$java_root")"; then
    printf '%s\n' "$java_path"
    return 0
  fi

  archive="$DOWNLOAD_DIR/temurin-jdk-25-$adoptium_os-$adoptium_arch.tar.gz"
  url="https://api.adoptium.net/v3/binary/latest/25/ga/$adoptium_os/$adoptium_arch/jdk/hotspot/normal/eclipse"
  echo "Downloading Java 25 for $adoptium_os $adoptium_arch..." >&2
  rm -rf "$java_root"
  mkdir -p "$java_root"
  download "$url" "$archive"
  tar -xzf "$archive" -C "$java_root"
  java_path="$(find_java_in "$java_root")" || fail "downloaded Java archive did not contain bin/java." 3
  printf '%s\n' "$java_path"
}

get_java25() {
  if [ "${SHAFT_MCP_FORCE_BOOTSTRAP_JAVA:-0}" != "1" ]; then
    if [ -n "${JAVA_HOME:-}" ] && [ -x "$JAVA_HOME/bin/java" ] && is_java25 "$JAVA_HOME/bin/java"; then
      printf '%s\n' "$JAVA_HOME/bin/java"
      return 0
    fi
    if command -v java >/dev/null 2>&1; then
      path_java="$(command -v java)"
      if is_java25 "$path_java"; then
        printf '%s\n' "$path_java"
        return 0
      fi
    fi
    if java_path="$(find_java_in "$TOOLS_DIR/jdk")"; then
      printf '%s\n' "$java_path"
      return 0
    fi
  fi
  download_java25
}

java_path="$(get_java25)"
java_bin="$(CDPATH= cd -- "$(dirname -- "$java_path")" && pwd -P)"
JAVA_HOME="$(CDPATH= cd -- "$java_bin/.." && pwd -P)"
export JAVA_HOME
PATH="$java_bin:$PATH"
export PATH

maven_ok() {
  output="$("$1" --version 2>&1)" || return 1
  version="$(printf '%s\n' "$output" | sed -n 's/.*Apache Maven \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\1 \2/p' | head -n 1)"
  [ -n "$version" ] || return 1
  major="${version%% *}"
  minor="${version#* }"
  [ "$major" -gt 3 ] || { [ "$major" -eq 3 ] && [ "$minor" -ge 9 ]; }
}

verify_sha512() {
  archive="$1"
  checksum="$2"
  expected="$(sed 's/[[:space:]].*$//' "$checksum" | head -n 1 | tr 'A-F' 'a-f')"
  if command -v sha512sum >/dev/null 2>&1; then
    actual="$(sha512sum "$archive" | awk '{print tolower($1)}')"
  elif command -v shasum >/dev/null 2>&1; then
    actual="$(shasum -a 512 "$archive" | awk '{print tolower($1)}')"
  elif command -v openssl >/dev/null 2>&1; then
    actual="$(openssl dgst -sha512 "$archive" | awk '{print tolower($NF)}')"
  else
    echo "Skipping Maven SHA-512 verification because no checksum tool is available." >&2
    return 0
  fi
  [ "$actual" = "$expected" ] || fail "checksum verification failed for $archive." 3
}

download_maven() {
  maven_home="$TOOLS_DIR/maven/apache-maven-$MAVEN_VERSION"
  maven="$maven_home/bin/mvn"
  if [ -x "$maven" ] && maven_ok "$maven"; then
    printf '%s\n' "$maven"
    return 0
  fi

  archive="$DOWNLOAD_DIR/apache-maven-$MAVEN_VERSION-bin.tar.gz"
  checksum="$archive.sha512"
  url="https://archive.apache.org/dist/maven/maven-3/$MAVEN_VERSION/binaries/apache-maven-$MAVEN_VERSION-bin.tar.gz"
  echo "Downloading Apache Maven $MAVEN_VERSION..." >&2
  download "$url" "$archive"
  download "$url.sha512" "$checksum"
  verify_sha512 "$archive" "$checksum"
  rm -rf "$maven_home"
  mkdir -p "$TOOLS_DIR/maven"
  tar -xzf "$archive" -C "$TOOLS_DIR/maven"
  [ -x "$maven" ] && maven_ok "$maven" || fail "downloaded Maven $MAVEN_VERSION is not executable." 3
  printf '%s\n' "$maven"
}

get_maven() {
  if [ "${SHAFT_MCP_FORCE_BOOTSTRAP_MAVEN:-0}" != "1" ] && command -v mvn >/dev/null 2>&1; then
    path_maven="$(command -v mvn)"
    if maven_ok "$path_maven"; then
      printf '%s\n' "$path_maven"
      return 0
    fi
  fi
  download_maven
}

maven_path="$(get_maven)"
maven_bin="$(CDPATH= cd -- "$(dirname -- "$maven_path")" && pwd -P)"
PATH="$maven_bin:$PATH"
export PATH

jar_directory="${TMPDIR:-/tmp}/shaft-mcp-bootstrap"
mkdir -p "$jar_directory"

echo "Resolving $ARTIFACT:$VERSION..." >&2
(
  cd "$WORK_DIR"
  "$maven_path" \
    --batch-mode \
    --no-transfer-progress \
    -U \
    -N \
    "$DEPENDENCY_GOAL" \
    "-Dartifact=$ARTIFACT:$VERSION" \
    "-DoutputDirectory=$jar_directory" \
    "-Dmdep.stripVersion=true" \
    "-Dmdep.overWriteReleases=true"
)

jar="$jar_directory/shaft-mcp.jar"
[ -f "$jar" ] || fail "Maven did not copy shaft-mcp.jar into $jar_directory." 4

echo "Configuring shaft-mcp for ${target#--}..." >&2
exec "$java_path" -jar "$jar" install "$target"
