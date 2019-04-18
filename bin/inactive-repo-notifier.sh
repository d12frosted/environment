#!/usr/bin/env bash

# Supports only master branch. And remote named origin.
#
# Notification is not posted if notification file already contains specified
# message.

set -e

REMOTE_URL="$1"
REPO_PATH="$2"
NOTIFICATION_FILE="$3"
MESSAGE="$4"
THRESHOLD_SECONDS="$5"

function log {
  echo "[$(date '+%d/%m/%y %H:%M:%S')]" "$@"
}

repo_name=${REMOTE_URL#*:}
repo_name=${repo_name%.git}

log "Checking $repo_name for being inactive"

if [[ ! -d "$REPO_PATH/.git" ]]; then
  log "Missing local copy of repository"
  git clone --depth 1 "$REMOTE_URL" "$REPO_PATH"
fi

cd "$REPO_PATH" && {
  git fetch --all
  git reset --hard origin/master

  now=$(date +%s)
  last=$(git show -s --format=%ct HEAD)
  diff=$(($now - $last))
  if [[ $diff -ge $THRESHOLD_SECONDS ]]; then
    log "No updates for $diff seconds."
    if [[ $(cat "$NOTIFICATION_FILE" | grep "$MESSAGE") ]]; then
      echo "Notification already exists, so not creating new one."
    else
      echo "Creating notification."
      echo "$MESSAGE" >> "$NOTIFICATION_FILE"
    fi
  else
    log "Last update was $diff seconds ago. Repo is active."
  fi
}
