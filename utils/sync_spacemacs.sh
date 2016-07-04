#!/usr/bin/env bash
#
# Sync spacemacs fork with original repository
#

# Configs
#

REPO=~/.spacemacs
BRANCH=develop
REMOTE_ORIGIN=origin
REMOTE_NAME=checkversion
REMOTE_URL="git@github.com:syl20bnr/spacemacs.git"

# Better defaults

pushd () {
  command pushd "$@" > /dev/null
}

popd () {
  command popd > /dev/null
}

# Implementation
#

set -e
pushd "$(pwd)"
cd $REPO
git diff-index --quiet HEAD -- || {
  echo "Your working directory is not clean."
  echo "Please commit or stash all changes before proceeding."
  exit 1
}
branch=$(git symbolic-ref --short HEAD)
if [[ $branch != "$BRANCH" ]]; then
  echo "Switching from $branch to $BRANCH"
  git checkout $BRANCH
fi
if [[ -d $REPO/.git/refs/remotes/$REMOTE_NAME ]]; then
  url=$(git remote get-url $REMOTE_NAME)
  if [[ $url != "$REMOTE_URL" ]]; then
    echo "Remote '$BRANCH' has wrong url, so updating it"
    echo "  $url -> $REMOTE_URL"
    git remote set-url $REMOTE_NAME $REMOTE_URL
  fi
else
  echo "Could not find remote '$REMOTE_NAME', so adding it"
  git remote add $REMOTE_NAME $REMOTE_URL
fi
git fetch $REMOTE_NAME
if [[ $(git rev-parse HEAD) == $(git rev-parse $REMOTE_NAME/$BRANCH) ]]; then
   echo "Everything up-to-date"
   exit 0
fi
echo "Fetched changes:"
git --no-pager lg HEAD..$REMOTE_NAME/$BRANCH
git reset --hard $REMOTE_NAME/$BRANCH
git push $REMOTE_ORIGIN $BRANCH
popd
