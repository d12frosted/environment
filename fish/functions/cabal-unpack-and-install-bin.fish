function cabal-unpack-and-install-bin -a package -d "Unpack and install specified executable package from cabal."
  set -l current_dir (pwd)
  cd $TMPDIR
  set -l dir $package*

  if test (count $dir) -ne 0
    echo "Found $TMPDIR$dir"
    echo "Looks like the package already unpacked in \$TMPDIR"
    cd $current_dir
    return 1
  end

  cabal unpack $package

  if test $status -ne 0
    cd $current_dir
    return 1
  end

  set -l dir $package*

  cd $TMPDIR/$dir

  cabal-install-bin

  cd $TMPDIR
  rm -rf $dir

  cd $current_dir
end
