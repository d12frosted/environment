#!/bin/bash
# Claude Code status line.
# Fields pulled in ONE jq call; colors are literal strings (no subshells).

input=$(cat)

IFS=$'\t' read -r cwd model ctx_pct fh_pct fh_reset wk_pct wk_reset < <(
  jq -r '[
    .workspace.current_dir // "",
    .model.display_name // "",
    (.context_window.used_percentage // ""),
    (.rate_limits.five_hour.used_percentage // ""),
    (.rate_limits.five_hour.resets_at // ""),
    (.rate_limits.seven_day.used_percentage // ""),
    (.rate_limits.seven_day.resets_at // "")
  ] | @tsv' <<< "$input"
)

E=$'\033'
C_DIR="${E}[36m"
C_BRANCH="${E}[35m"
C_MODEL=""
C_DIM="${E}[38;5;242m"
C_GREEN="${E}[32m"
C_YELLOW="${E}[33m"
C_RED="${E}[31m"
C_RESET="${E}[0m"

display_dir=${cwd/#$HOME/\~}

git_seg=""
if git -C "$cwd" rev-parse --git-dir >/dev/null 2>&1; then
  branch=$(git -C "$cwd" --no-optional-locks branch --show-current 2>/dev/null)
  if [ -z "$branch" ]; then
    branch=$(git -C "$cwd" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
  fi
  if [ -n "$branch" ]; then
    if git -C "$cwd" --no-optional-locks diff-index --quiet HEAD -- 2>/dev/null; then
      dirty="${C_DIM}✓${C_RESET}"
    else
      dirty="${C_YELLOW}×${C_RESET}"
    fi
    git_seg=" ${C_DIM}on${C_RESET} ${C_BRANCH}${branch}${C_RESET} ${dirty}"
  fi
fi

now=$(date +%s)

fmt_reset() {
  local diff=$(( $1 - now ))
  (( diff <= 0 )) && { printf "now"; return; }
  local d=$((diff/86400)) h=$(((diff%86400)/3600)) m=$(((diff%3600)/60))
  if   (( d > 0 )); then printf "%dd%dh" "$d" "$h"
  elif (( h > 0 )); then printf "%dh%dm" "$h" "$m"
  else                   printf "%dm"    "$m"
  fi
}

pct_color() {
  local p=${1%.*}
  if   (( p >= 80 )); then printf "%s" "$C_RED"
  elif (( p >= 50 )); then printf "%s" "$C_YELLOW"
  else                     printf "%s" "$C_GREEN"
  fi
}

# Segment with pace-aware coloring.
# When a window length is given, color reflects the projected endpoint
# (used% / elapsed fraction), not the raw percentage — so 40% one day
# into a 7-day window reads red, not green.
seg() {
  local label=$1 pct=$2 reset=$3 window=$4
  [ -z "$pct" ] && return
  local p=${pct%.*}
  local color="" projected=""

  if [ -n "$reset" ] && [ -n "$window" ]; then
    local elapsed=$(( window - (reset - now) ))
    # Skip projection when the window just started — the multiplier
    # explodes and is meaningless. Fall back to raw coloring.
    if (( elapsed >= window / 20 )); then
      projected=$(( p * window / elapsed ))
      if   (( projected >= 100 )); then color="$C_RED"
      elif (( projected >=  80 )); then color="$C_YELLOW"
      else                              color="$C_GREEN"
      fi
    fi
  fi

  [ -z "$color" ] && color=$(pct_color "$p")

  local out=" ${C_DIM}·${C_RESET} ${C_DIM}${label}${C_RESET} ${color}${p}%${C_RESET}"
  if [ -n "$projected" ]; then
    (( projected > 999 )) && projected=999
    out+="${C_DIM}→${C_RESET}${color}${projected}%${C_RESET}"
  fi
  if [ -n "$reset" ]; then
    out+=" ${C_DIM}($(fmt_reset "$reset"))${C_RESET}"
  fi
  printf "%s" "$out"
}

ctx_seg=$(seg "ctx" "$ctx_pct" "" "")
fh_seg=$(seg  "5h"  "$fh_pct"  "$fh_reset" 18000)
wk_seg=$(seg  "wk"  "$wk_pct"  "$wk_reset" 604800)

printf "%s%s%s%s %s%s%s%s%s%s" \
  "$C_DIR" "$display_dir" "$C_RESET" "$git_seg" \
  "$C_MODEL" "$model" "$C_RESET" \
  "$ctx_seg" "$fh_seg" "$wk_seg"
