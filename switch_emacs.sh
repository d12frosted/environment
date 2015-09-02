#!/bin/sh

case $1 in
    "d12")
        emacs_path=~/.environment/emacs
	      ;;

    "spacemacs")
	      emacs_path=~/.emacs.d.spacemacs
	      ;;

    "purcell")
	      emacs_path=~/.emacs.d.purcell
	      ;;

    *)
	      echo "unknown type"
	      exit 1
	      ;;
esac

echo "Switching from '$(readlink ~/.emacs.d)'
            to '$emacs_path'"
rm -rf ~/.emacs.d
ln -s $emacs_path ~/.emacs.d
