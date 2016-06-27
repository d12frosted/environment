function spacemacs_update -d "Update spacemacs"
  set -l dlocation ~/.spacemacs
  set -l dbranch "develop"
  set -l dremote "checkversion"
  set -l durl "git@github.com:syl20bnr/spacemacs.git"
  pushd (pwd)
  cd $dlocation
  git diff-index --quiet HEAD --
  if test $status -ne 0
    echo "Your working directory is not clean. Please commit or stash any changes before proceeding."
    return 1
  end
  set -l branch (git symbolic-ref --short HEAD)
  if test $branch != $dbranch
    echo "Switching from $branch to $dbranch"
    git checkout $dbranch; or return
  end
  if test -d $dlocation/.git/refs/remotes/$dremote
    set -l real_url (git remote get-url $dremote)
    if test $durl != $real_url
      echo "Remote '$dremote' has wrong url, so updating it"
      echo "  $real_url -> $durl"
      git remote set-url $dremote $durl
    end
  else
    echo "Could not find remote '$dremote', so adding it"
    git remote add $dremote $durl
  end
  git fetch $dremote
  echo "Fetched changes:"
  git --no-pager lg HEAD..$dremote/$dbranch
  git reset --hard $dremote/$dbranch
  git push origin $dbranch
  popd
end
