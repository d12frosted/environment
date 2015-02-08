#!/usr/bin/env zsh

myself="d12frosted"
envdir="$HOME/.environment"

autoload colors
colors
for COLOR in RED GREEN YELLOW BLUE MAGENTA CYAN BLACK WHITE; do
    eval $COLOR='$fg_no_bold[${(L)COLOR}]'
    eval BOLD_$COLOR='$fg_bold[${(L)COLOR}]'
done
eval RESET='$reset_color'

starting_dir=$pwd

# some useful functions

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

pull() {
    cd $1
    git pull
    cd $envdir
}

# check user

if [[ $USER = $myself ]]; then
    log "-------------------------------------------------------------------------------"
    log "Hello myself! Don't forget to setup ssh keys on https://github.com/settings/ssh"
    log "Because your repos are going to be cloned using ssh links instead of https"
    log "-------------------------------------------------------------------------------\n"
else
    log "Hello $USER!"
fi

log "Welcome to environment setup hell!"

# check operating system

log "Running on $(uname -s)"

osx=true
if [[ $(uname -s) != "Darwin" ]]; then
    osx=false
fi

# define dependencies list

if [[ $osx = true ]] ; then
    # they come out of box on OS X
    # but we want to check to be sure
    dependencies=(git curl ruby)
else
    dependencies=(git curl emacs-24.4 fish)
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

log "Everything is fine. Start install."

if [[ -d $envdir ]]; then
    log "Looks like you already has environment repository. Update it..."
    pull $envdir
else
    log "First, we need to clone environment repository."
    clone environment $envdir
fi

# install ghc and cabal

separator
hash ghc || {
    ghcversion="7.8.3"

    if [[ -e $HOME/.ghc ]]; then
        log "Moving your ~/.ghc to ~/old.ghc"
        mv $HOME/{,old}.ghc
    fi

    archi=$(uname -m)
    if [[ $(uname -s) = "Darwin" ]]; then
        os="apple-darwin"
    else
        if [[ $archi = "i686" ]]; then
            archi=i386
        fi
        os="unknown-linux-deb7"
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
}

hash cabal || {
    cabalversion="1.22.0.0"

    if [[ -e $HOME/.cabal ]]; then
        log "Moving your ~/.cabal to ~/old.cabal"
        mv $HOME/{,old}.cabal
    fi

    archi=$(uname -m)
    if [[ $(uname -s) = "Darwin" ]]; then
        os="apple-darwin-mavericks"
    else
        if [[ $archi = "i686" ]]; then
            archi=i386
        fi
        cabalversion="1.20.0.1"
        os="unknown-linux"
    fi

    tmpdir=/tmp/install-haskell
    mkdir -p $tmpdir


    cd $tmpdir
    log "Downloading cabal..."
    cabaltar=cabal-${cabalversion}-${archi}-${os}.tar.gz
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
}

log "Install useful cabal binaries"
cabal install -j alex happy
log "Install basic prelude"
cabal install basic-prelude

# install brew

if [[ $osx = true ]] ; then
    separator

    hash brew || {
        log "Install brew"
        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    }

    log "Update brew"
    brew update
fi

# install fish

hash fish || {
    separator
    log "Install latest version of fish"
    git clone https://github.com/fish-shell/fish-shell.git $HOME/.fish-shell
    cd $HOME/.fish-shell

    autoconf
    ./configure
    make
    make install

    if grep -q $(which fish) "/etc/shells"; then
        log 'Fish is already in "/etc/shells" file'
    else
        sudo sh -c 'echo $(which fish) >> /etc/shells'
    fi
    sudo chsh -s $(which fish)
}

cd $envdir

# install emacs

hash emacs-24.4 || {
    separator
    log "Install latest version of emacs"
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
cd $starting_dir

log "Install haskell-mode for emacs"
cd $emacsd/packages/haskell-mode
make

log "Install structured-haskell-mode for emacs"
cd $emacsd/packages/structured-haskell-mode
cabal install
cd $emacsd/packages/structured-haskell-mode/elisp
make

log "Install some other cabal packages for happy haskell coding"
cabal -j install hasktags haskell-docs present
