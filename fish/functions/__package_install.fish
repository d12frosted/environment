function __package_install -a pkg -d "Install a package on OS X using brew"
  hash $pkg > /dev/null 2>&1; or begin
    echo "Warning! '$pkg' is not installed!"
    if [ (uname -s) = "Darwin" ]
      echo "Installing from brew."
      brew install $pkg
    else
      echo "Don't know how to install packages on your system! Get yourself a mac!"
      exit 1
    end
  end
end
