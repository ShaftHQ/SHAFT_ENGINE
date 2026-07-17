#!/usr/bin/env bash
# Global Claude Code status line:
#   dir · git-branch · model (effort) · ctx-used/ctx-max (P%) · in-tok/out-tok · +added/-removed
# Reads the statusLine JSON payload on stdin. No jq dependency; parses fields
# with grep/sed/awk. Reads .git/HEAD directly (no `git` subprocess) for the
# branch, and the session transcript for cumulative input/output tokens
# consumed (summed across every turn, not just the latest). No cost/$ shown.

input="$(cat)"

# --- helpers ---------------------------------------------------------------
# Extract a top-level-or-nested "key":"string" value from the payload.
json_str() { printf '%s' "$input" | grep -o "\"$1\"[[:space:]]*:[[:space:]]*\"[^\"]*\"" | head -1 | sed -E 's/.*:[[:space:]]*"([^"]*)"/\1/'; }
# Extract a "key":number value from the payload.
json_num() { printf '%s' "$input" | grep -o "\"$1\"[[:space:]]*:[[:space:]]*[0-9.]*" | head -1 | sed -E 's/.*:[[:space:]]*([0-9.]*)/\1/'; }
# Compact a raw token count into "12.3k"/"1.2M"; passes small numbers through as-is.
fmt_tokens() {
  local n="${1:-0}"
  case "$n" in ''|*[!0-9]*) printf '0'; return ;; esac
  if [ "$n" -ge 1000000 ]; then
    awk -v n="$n" 'BEGIN { printf "%.1fM", n / 1000000 }'
  elif [ "$n" -ge 1000 ]; then
    awk -v n="$n" 'BEGIN { printf "%.1fk", n / 1000 }'
  else
    printf '%s' "$n"
  fi
}

model="$(json_str display_name)"; [ -z "$model" ] && model="Claude"
model_id="$(json_str id)"
cwd="$(json_str current_dir)"; [ -z "$cwd" ] && cwd="$(json_str cwd)"

# --- effort level ------------------------------------------------------------
# Scoped to the "effort" object specifically so it never picks up an unrelated
# "level" key elsewhere in the payload. Absent entirely on models without an
# effort parameter -- the segment is simply skipped in that case.
effort="$(printf '%s' "$input" | grep -o '"effort"[[:space:]]*:[[:space:]]*{[^}]*"level"[[:space:]]*:[[:space:]]*"[^"]*"' | sed -E 's/.*"level"[[:space:]]*:[[:space:]]*"([^"]*)".*/\1/')"

# --- directory ---------------------------------------------------------------
cwd_norm="${cwd//\\//}"
dirbase="${cwd_norm##*/}"
[ -z "$dirbase" ] && dirbase="$cwd_norm"
[ -z "$dirbase" ] && dirbase="?"

# --- git branch (walk up for .git, read HEAD directly) -----------------------
dir="$cwd_norm"; gitdir=""
while [ -n "$dir" ]; do
  if [ -f "$dir/.git" ]; then
    gitdir="$(sed -E 's/^gitdir: *//' "$dir/.git" 2>/dev/null)"; break
  elif [ -d "$dir/.git" ]; then
    gitdir="$dir/.git"; break
  fi
  parent="${dir%/*}"; [ "$parent" = "$dir" ] && break; dir="$parent"
done
branch=""
if [ -n "$gitdir" ] && [ -f "$gitdir/HEAD" ]; then
  head_content="$(cat "$gitdir/HEAD" 2>/dev/null)"
  case "$head_content" in
    ref:*) branch="${head_content#ref: refs/heads/}" ;;
    *) branch="${head_content:0:7}" ;;
  esac
fi

# --- context size (current window occupancy, not a %-only guess) ------------
# Prefers the payload's own context_window.* fields (accurate, pre-computed by
# Claude Code itself). Falls back to summing the transcript's latest usage
# entry against a model-id-based window guess if the payload predates those
# fields, so this degrades gracefully on an older CLI instead of going blank.
ctx=""
window_size="$(json_num context_window_size)"
used_pct="$(json_num used_percentage)"
ctx_in="$(json_num total_input_tokens)"
ctx_out="$(json_num total_output_tokens)"
if [ -n "$window_size" ]; then
  occupied=$(( ${ctx_in:-0} + ${ctx_out:-0} ))
  [ -z "$used_pct" ] && [ "$window_size" -gt 0 ] 2>/dev/null && used_pct=$(( occupied * 100 / window_size ))
  ctx="$(fmt_tokens "$occupied")/$(fmt_tokens "$window_size") ctx (${used_pct}%)"
else
  transcript="$(json_str transcript_path)"; transcript="${transcript//\\\\/\\}"; transcript="${transcript//\\//}"
  if [ -n "$transcript" ] && [ -f "$transcript" ]; then
    usage="$(grep -o '"usage":{[^}]*}' "$transcript" | tail -1)"
    if [ -n "$usage" ]; then
      it="$(printf '%s' "$usage" | grep -o '"input_tokens":[0-9]*' | grep -o '[0-9]*')"
      cc="$(printf '%s' "$usage" | grep -o '"cache_creation_input_tokens":[0-9]*' | grep -o '[0-9]*')"
      cr="$(printf '%s' "$usage" | grep -o '"cache_read_input_tokens":[0-9]*' | grep -o '[0-9]*')"
      occupied=$(( ${it:-0} + ${cc:-0} + ${cr:-0} ))
      case "$model_id" in *1m*|*1M*) window_size=1000000 ;; *) window_size=200000 ;; esac
      [ "$occupied" -gt 0 ] && ctx="$(fmt_tokens "$occupied")/$(fmt_tokens "$window_size") ctx ($(( occupied * 100 / window_size ))%)"
    fi
  fi
fi

# --- cumulative session tokens consumed (summed across every turn) ----------
# context_window.total_input_tokens/total_output_tokens reflect only the
# *current* window's occupancy, not the session's cumulative consumption, so
# this sums every "usage" block in the transcript instead of reading one field.
tokens=""
transcript="$(json_str transcript_path)"; transcript="${transcript//\\\\/\\}"; transcript="${transcript//\\//}"
if [ -n "$transcript" ] && [ -f "$transcript" ]; then
  read -r sum_in sum_out <<EOF
$(grep -o '"usage":{[^}]*}' "$transcript" | awk '
    {
      it = 0; ot = 0; cc = 0; cr = 0
      if (match($0, /"input_tokens":[0-9]+/)) { s = substr($0, RSTART, RLENGTH); sub(/.*:/, "", s); it = s + 0 }
      if (match($0, /"output_tokens":[0-9]+/)) { s = substr($0, RSTART, RLENGTH); sub(/.*:/, "", s); ot = s + 0 }
      if (match($0, /"cache_creation_input_tokens":[0-9]+/)) { s = substr($0, RSTART, RLENGTH); sub(/.*:/, "", s); cc = s + 0 }
      if (match($0, /"cache_read_input_tokens":[0-9]+/)) { s = substr($0, RSTART, RLENGTH); sub(/.*:/, "", s); cr = s + 0 }
      total_in += it + cc + cr
      total_out += ot
    }
    END { printf "%d %d", total_in, total_out }
  ')
EOF
  if [ -n "$sum_in" ] && [ "$(( ${sum_in:-0} + ${sum_out:-0} ))" -gt 0 ]; then
    tokens="in $(fmt_tokens "$sum_in")/out $(fmt_tokens "$sum_out")"
  fi
fi

# --- lines changed (no $ cost shown) -----------------------------------------
added="$(json_num total_lines_added)"; removed="$(json_num total_lines_removed)"
lines=""
if [ -n "$added$removed" ] && [ "$(( ${added:-0} + ${removed:-0} ))" -gt 0 ]; then
  lines="+${added:-0}/-${removed:-0}"
fi

# --- assemble (middot separators, skip empty segments) ------------------------
sep=$' \xc2\xb7 '
modelseg="$model"
[ -n "$effort" ] && modelseg="$model ($effort)"
out="$dirbase"
[ -n "$branch" ] && out="$out$sep$branch"
out="$out$sep$modelseg"
[ -n "$ctx" ]    && out="$out$sep$ctx"
[ -n "$tokens" ] && out="$out$sep$tokens"
[ -n "$lines" ]  && out="$out$sep$lines"
printf '%s' "$out"
