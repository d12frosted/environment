function cabal-install-bin -d "Install executables from .cabal file in current directory"
  # set some color settings
  set -l error_color red
  set -l msg_color blue

  # get cabal file in current directory
  set -l cb *.cabal

  set -l c (count *.cabal)
  # we expect only 1 cabal file to be existing
  if test c -ne 1
    set_color $error_color
    if test c -eq 0
      echo "Couldn' find cabal file in (pwd)"
    else
      echo "Found $c cabal files. Think about it!"
    end
    set_color normal
    return 1
  end

  set_color $msg_color
  echo "Using $cb"

  # check if sandbox is not created yet
  if test ! \( -e .cabal-sandbox \) -o ! \( -e cabal.sandbox.config \)
    echo "It looks like there is no sandbox, so creating one"
    set_color normal
    # create sandbox
    cabal sandbox init
  end

  # todo add support of multiple executables
  set -l name (cabal info *.cabal | sed -ne "s/ *Executables: *\(.*\)/\1/p")

  # check that the name is not empty
  if test ! \( -n $name \)
    set_color $error_color
    echo "Couldn't find any executable in cabal file"
    set_color normal
    return 1
  end

  set_color $msg_color
  echo "Found executables: $name"
  echo "Installing dependencies"
  set_color normal

  # first we want to install dependencies
  # we could just `cabal install`
  # but I find separate installation
  # more satisfying
  cabal install --only-dependencies

  if test $status -ne 0
    return 1
  end

  set_color $msg_color
  echo "Building application"
  set_color normal

  # install package
  cabal install

  if test $status -ne 0
    return 1
  end

  set_color $msg_color
  echo "Copying $name to ~/.bin"
  set_color normal
  # now copy executable to ~/.bing
  cp ".cabal-sandbox/bin/$name" "$HOME/.bin/$name"
end
