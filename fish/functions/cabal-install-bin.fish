function cabal-install-bin
  # set some color settings
  set -l error_color red
  set -l msg_color blue

  # get cabal file in current directory
  set -l cb *.cabal

  # we expect only 1 cabal file to be existing
  if test (count *.cabal) -ne 1
    set_color $error_color
    echo "Couldn't find cabal file, probably wrong directory"
    set_color normal
  else
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
    if test -n $name
      set_color $msg_color
      echo "Found executables: $name"
      echo "Installing dependencies"
      set_color normal

      # first we want to install dependencies
      # we could just `cabal install`
      # but I find separate installation
      # more satisfying
      cabal install --only-dependencies

      # todo check that cabal install
      # didn't fail

      set_color $msg_color
      echo "Building application"
      set_color normal

      # install package
      cabal install

      # todo check that cabal install
      # didn't fail

      set_color $msg_color
      echo "Copying $name to ~/.bin"
      set_color normal
      # now copy executable to ~/.bing
      cp ".cabal-sandbox/bin/$name" "$HOME/.bin/$name"
    else
      set_color $error_color
      echo "Couldn't find any executable in cabal file"
      set_color normal
    end
  end
end
