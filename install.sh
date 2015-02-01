#!/usr/bin/env zsh

myself="d12frosted"

autoload colors
colors
for COLOR in RED GREEN YELLOW BLUE MAGENTA CYAN BLACK WHITE; do
    eval $COLOR='$fg_no_bold[${(L)COLOR}]'
    eval BOLD_$COLOR='$fg_bold[${(L)COLOR}]'
done
eval RESET='$reset_color'

startingDir=$pwd

# some printing functions

log() {
    echo "${CYAN}$*${RESET}"
}

warn() {
    echo "${YELLOW}Warning! $*${RESET}"
}

error() {
    echo "${RED}Error! $*${RESET}"
    exit 1
}

separator() {
    echo "\n${GREEN}****************${RESET}\n"
}

# check user

if [[ $USER = $myself ]]; then
    log "-------------------------------------------------------------------------------"
    log "Hello myself! Don't forget to setup ssh keys on https://github.com/settings/ssh"
    log "Because your repos are going to be cloned using ssh links instead of https"
    log "-------------------------------------------------------------------------------\n"
else
    log "Hello $USER!"
    log "Welcome to environment setup hell!"
fi

clone() {
    repo=$1
    dir=$2
    if [[ $USER = $myself ]]; then
        url="git@github.com:${myself}/${repo}.git"
    else
        url="https://github.com/${myself}/${repo}.git"
    fi
    log "Clone $url to $dir"
    git clone $url $dir
}

# check operating system

log "Running on $(uname -s)"

osx=true
if [[ $(uname -s) != "Darwin" ]]; then
    osx=false
fi

# define dependencies list

if [ $osx = true ] ; then
    # they come out of box on OS X
    # but we want to check to be sure
    dependencies=(git curl ruby)
else
    dependencies=(git curl emacs-24.4)
    warn "Looks like your're not on OS X. Most probably you need to install some dependencies before this script will work for you."
    separator
fi

# check dependencies

log "Check for dependencies: ${GREEN}$dependencies${RESET}"

for p in $dependencies; do
    hash $p 2>/dev/null || {
        error "Required but not found: '$p'"
    }
done

log "Everything is fine. Start installing."

# install oh-my-zsh

separator
log "Install oh-my-zsh"

if [ -d "$ZSH" ]; then
    log "oh-my-zsh is already installed"
    /bin/sh $ZSH/tools/upgrade.sh
else
    curl -L http://install.ohmyz.sh | sh
fi

# instal d12frosted-zshrc

separator
log "Install zsh settings"

zshrc=~/.d12frosted-zshrc

if [ -d "$zshrc" ]; then
    log "Looks like d12frosted-zshrc is alreadyy installed"
    log "Updating d12frosted-zshrc"
    cd $zshrc
    git pull
else
    clone .d12frosted-zshrc $zshrc
fi

cd $zshrc
zsh install.sh
cd $startingDir

# install brew

if [ $osx = true ] ; then
    separator

    hash brew || {
        log "Installing brew"
        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    }

    log "Updating brew"
    brew update
fi

# install emacs

hash emacs-24.4 || {
    separator
    log "Installing latest version of emacs"
    brew install --cocoa --srgb emacs
    ln -s /usr/local/Cellar/emacs/24.4/Emacs.app /Applications
}

emacs --version

# install .emacs.d

separator
log "Install .emacs.d"

emacsd=~/.emacs.d

if [ -d "$emacsd" ]; then
    log "Looks like emacs configs are already installed"
    log "Updating emacs configs"
    cd $emacsd
    git pull
else
    clone d12frosted-emacs $emacsd
fi

cd $emacsd
git submodule update --init
cd $startingDir

# install ghc and cabal
# for more generic version see https://github.com/yogsototh/install-haskell
# thanks, Yann!

separator
log "Install ghc and cabal"

if [[ -e $HOME/.cabal ]]; then
    log "Moving your ~/.cabal to ~/old.cabal"
    mv $HOME/{,old}.cabal
fi

if [[ -e $HOME/.ghc ]]; then
    log "Moving your ~/.ghc to ~/old.ghc"
    mv $HOME/{,old}.ghc
fi

ghcversion="7.8.3"
cabalversion="1.22.0.0"
archi=$(uname -m)
if [[ $(uname -s) = "Darwin" ]]; then
    os="apple-darwin"
    cabalos="apple-darwin-mavericks"
else
    if [[ $archi = "i686" ]]; then
        archi=i386
    fi
    cabalversion="1.20.0.1"
    os="unknown-linux-deb7"
    cabalos="unknown-linux"
    # -------------------------
    # apt-get install libgmp-dev
fi

tmpdir=/tmp/install-haskell
mkdir -p $tmpdir

cd $tmpdir
ghctar=ghc-${ghcversion}-${archi}-${os}.tar.xz
if [[ ! -e $ghctar ]]; then
    log "Downloading GHC..."
    curl -LO http://www.haskell.org/ghc/dist/${ghcversion}/$ghctar
else
    log "Using already downloaded GHC ($tmpdir)..."
fi
log "Installing GHC..."
tar xJf $ghctar
cd ghc-${ghcversion}
./configure && make install

cd $tmpdir
log "Downloading cabal..."
cabaltar=cabal-${cabalversion}-${archi}-${cabalos}.tar.gz
[[ $cabalos = "unknown-linux" ]] && cabaltar=cabal-${archi}-${cabalos}.tar.gz
if [[ ! -e $cabaltar ]]; then
    curl -LO http://www.haskell.org/cabal/release/cabal-install-$cabalversion/$cabaltar
else
    log "Using already downloaded cabal ($tmpdir)..."
fi
tar xzf $cabaltar
log "Installing cabal..."
if [[ -e ./cabal ]]; then
    mv cabal /usr/local/bin
else
    mv ./dist/build/cabal/cabal /usr/local/bin
fi

log "Init cabal..."
sudo -u $normaluser cabal info >/dev/null 2>&1

log "Run cabal update"
cabal update

log "Install useful cabal binaries"
cabal install -j alex happy

log "Install basic prelude"
cabal install basic-prelude

# back to emacs configuration

separator

log "Install haskell-mode for emacs"
cd $emacsd/packages/haskell-mode
make

log "Install structured-haskell-mode for emacs"
cd $emacsd/packages/structured-haskell-mode
cabal install
cd $emacsd/packages/structured-haskell-mode/elisp
make

log "Install some other cabal packages for happy haskell coding"
cabal -j install hasktags haskell-docs present ghc-mod
