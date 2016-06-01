function spacemacs_update -d "Update spacemacs"
  set -l dbranch "develop"
  set -l dremote "checkversion"
  set -l durl "https://github.com/syl20bnr/spacemacs"
  pushd (pwd)
  cd ~/.emacs.d
  set -l branch (git symbolic-ref --short HEAD)
  if test $branch != $dbranch
    echo "Switching from $branch to $dbranch"
    git checkout $dbranch; or return
  end
  git ls-remote $dremote > /dev/null 2> /dev/null; or begin
    git remote add $dremote $durl
  end
  git fetch $dremote; and git rebase $dremote/$dbranch
  git push origin $dbranch
  popd
end
