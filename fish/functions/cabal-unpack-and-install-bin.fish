function cabal-unpack-and-install-bin
  set -l current_dir (pwd)
  set -l package $argv[1]

  cd $TMPDIR

  set -l dir $package*

  # this is simpel script
  # so just don't install anything
  # when there is already
  # dir for this package
  if test (count $dir) -ne 0
    echo "There is already folder for this package in $TMPDIR"
  else
    cabal unpack $package; and begin
      cd $TMPDIR/$dir

      cabal-install-bin

      # todo check that cabal install
      # didn't fail

      cd $TMPDIR
      rm -rf $dir
    end
  end
  cd $current_dir
end
