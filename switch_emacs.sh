#!/bin/sh

case $1 in
    "d12")
	      echo "d12"
	      rm -rf ~/.emacs.d
	      ln -s ~/.environment/emacs ~/.emacs.d
	      ;;

    "spacemacs")
	      echo "spacemacs"
	      rm -rf ~/.emacs.d
	      ln -s ~/.emacs.d.spacemacs ~/.emacs.d
	      ;;
    *)
	      echo "unknown type"
	      exit 1
	      ;;
esac
