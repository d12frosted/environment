# d12frosted environment

Be aware, that this repository is not designed for general use. But probably you can check some of its part and steal what interests you.

## Usage

Use it on your own risk as follows:

```sh
curl -L https://github.com/d12frosted/d12frosted-environment/install.sh | zsh
```

It can take a while, because it installs:

* oh-my-zsh
* my personal zsh configs
* brew (OS X only)
* emacs (OS X only)
* my personal emacs configs
* ghc and cabal (from sources)
* some cabal packages

## TODO

* Add custom .ghci file (with docs and stuff)
* Travis tests on ubuntu and os x
* Before making git pull - check for any existing changes
* Merge zshrc configs repo with this one
* `git submodule update --init` could be dangerous
