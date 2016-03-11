#!/usr/bin/env bash

function run {
  # how we want to extract the variables from the commit message.
  format_name="--format=%cn"
  format_when="--format=%cr"
  format_summary="--format=%s"
  format_body="--format=%b"


  # what repo do we want to watch (default to origin/master)
  if [ -z "$1" ]; then
    repo="origin"
  else
    repo="$1"
  fi

  if [ -z "$2" ]; then
    branch="master"
  else
    branch="$2"
  fi

  latest_revision="none"

  cd $dir

  # loop forever, need to kill the process.
  while [ 1 ]; do

    # get the latest revision SHA.
    git fetch $repo
    current_revision=$(git rev-parse $repo/$branch)

    # if we haven't seen that one yet, then we know there's new stuff.
    if [ $latest_revision != $current_revision ]; then

      # mark the newest revision as seen.
      latest_revision=$current_revision

      # extract the details from the log.
      commit_name=`git log -1 $format_name $latest_revision`
      commit_when=`git log -1 $format_when $latest_revision`
      commit_summary=`git log -1 $format_summary $latest_revision`
      commit_body=`git log -1 $format_body $latest_revision`

      # notify the user of the commit.
      title="$commit_name committed to $(pwd | sed s,$HOME,~,)"
      subtitle="$repo/$branch - $commit_when"
      # body="`echo $commit_summary && echo $commit_body`"
      terminal-notifier -title "$title" -subtitle "$subtitle" -message "$commit_summary"

    fi
    sleep 60
  done
}

if [ -z "$1" ]; then
  echo "Error: Must specify a directory" >&2
  exit 1
else
  dir="$1"
fi

set -e
cd $dir
set +e

LOG_FILE=git-notify.log

exec > >(tee -a ${LOG_FILE} )
exec 2> >(tee -a ${LOG_FILE} >&2)

if git rev-parse --git-dir > /dev/null 2>&1; then
  (run $2 $3 &)
else
  echo "Error: not a git repo"
fi
