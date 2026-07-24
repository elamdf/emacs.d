#!/usr/bin/env bash
set -euo pipefail

REMINDERS_LIST="Org TODO"
EMACS="${EMACS:-/opt/homebrew/bin/emacs}"
EXPORT_EL="$HOME/.emacs.d/scripts/org-reminders-export.el"
LOCK_DIR="${TMPDIR:-/tmp}/org-to-reminders.lock"
LOG_FILE="${TMPDIR:-/tmp}/org-to-reminders.log"

ORG_PATHS=(
    "$HOME/Documents/projects/build.org"
  "$HOME/Documents/projects/meetings"
)

if [[ "${1:-}" == "--background" ]]; then
  shift
  nohup "$0" "$@" >"$LOG_FILE" 2>&1 &
  printf 'org-to-reminders: started background sync as pid %s; log: %s\n' "$!" "$LOG_FILE"
  exit 0
fi

lock_acquired=0
tmp_tsv=""
tmp_unique_tsv=""
tmp_el=""

cleanup() {
  rm -f "$tmp_tsv" "$tmp_unique_tsv" "$tmp_el"
  if [[ "$lock_acquired" -eq 1 ]]; then
    rm -rf "$LOCK_DIR"
  fi
}

acquire_lock() {
  if mkdir "$LOCK_DIR" 2>/dev/null; then
    lock_acquired=1
    printf '%s\n' "$$" > "$LOCK_DIR/pid"
    return
  fi

  local pid=""
  if [[ -r "$LOCK_DIR/pid" ]]; then
    pid="$(<"$LOCK_DIR/pid")"
  fi

  if [[ "$pid" =~ ^[0-9]+$ ]] && kill -0 "$pid" 2>/dev/null; then
    printf 'org-to-reminders: another sync is already running as pid %s\n' "$pid" >&2
    exit 0
  fi

  rm -rf "$LOCK_DIR"
  if mkdir "$LOCK_DIR" 2>/dev/null; then
    lock_acquired=1
    printf '%s\n' "$$" > "$LOCK_DIR/pid"
    return
  fi

  printf 'org-to-reminders: could not acquire lock %s\n' "$LOCK_DIR" >&2
  exit 1
}

trap cleanup EXIT
acquire_lock

tmp_tsv="$(mktemp)"
tmp_unique_tsv="$(mktemp)"
tmp_el="$(mktemp)"

elisp_string() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  printf '"%s"' "$s"
}

org_files=()
for p in "${ORG_PATHS[@]}"; do
  if [[ -d "$p" ]]; then
    while IFS= read -r f; do
      org_files+=("$f")
    done < <(find "$p" -type f -name '*.org' -not -path '*/.git/*')
  elif [[ -f "$p" ]]; then
    org_files+=("$p")
  fi
done

{
  printf ';; -*- lexical-binding: t; -*-\n'
  printf '(load-file %s)\n' "$(elisp_string "$EXPORT_EL")"
  printf '(org-reminders-export-todos (list\n'
  for f in "${org_files[@]}"; do
    printf '  %s\n' "$(elisp_string "$f")"
  done
  printf ') %s)\n' "$(elisp_string "$tmp_tsv")"
} > "$tmp_el"

"$EMACS" --batch -Q --load "$tmp_el" >/dev/null

awk -F '\t' '!seen[$1]++' "$tmp_tsv" > "$tmp_unique_tsv"

osascript "$HOME/.emacs.d/scripts/org-to-reminders.applescript" "$REMINDERS_LIST" "$tmp_unique_tsv"
