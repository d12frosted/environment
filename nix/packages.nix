{ pkgs, lib, stdenv, ... }:

with pkgs;
let exe = haskell.lib.justStaticExecutables;
in [
  # pandoc
  coreutils
  eask
  ffmpeg
  fish
  fontconfig
  git
  git-filter-repo
  git-lfs
  gnugrep
  gnumake
  gnumeric
  gnupg
  gnuplot
  gnused
  gnutar
  hledger
  hledger-ui
  hledger-web
  home-manager
  hpack
  imagemagick
  jq
  killall
  kubectl
  libicns
  libwebp
  ncdu
  openssh
  openssl
  python310Packages.pygments
  ranger
  ripgrep
  rsync
  sqlite
  texlive.combined.scheme-full
  unrar
  unzip
  wget
] ++ lib.optionals stdenv.isDarwin [
  # xquartz
  # emacs
  pinentry_mac
  terminal-notifier
  youtube-dl
] ++ lib.optionals stdenv.isLinux [
  brave
  chrony
  docker
  dropbox-cli
  emacsGit
  entr
  feh
  firefox
  flameshot
  graphviz
  htop
  inxi
  jd-gui
  krita
  mysql80
  pcmanfm
  pinta
  qutebrowser
  scala
  scrot
  slack
  traceroute
  tree
  xdg-utils
  zip
] ++ [
  # all things editor
  (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
  languagetool
  nixfmt-classic
  nixpkgs-fmt
  ormolu
  shellcheck
] ++ [
  # all things haskell
  cabal2nix
  hpack
] ++ [
  # all things node :fear:
  nodePackages.npm
  nodePackages.pnpm
  nodePackages.typescript
  nodePackages.typescript-language-server
  nodePackages.webpack
  nodejs
  yarn
] ++ [
  # fonts
  font-awesome_4
  fontconfig
  # liberation_ttf
  mplus-outline-fonts.githubRelease
  source-code-pro
  source-sans-pro
  source-serif-pro
  symbola
  roboto-mono
] ++ lib.optionals stdenv.isLinux [
  # work
  globalprotect-openconnect
  jetbrains.idea-ultimate
  networkmanager-openconnect
  openconnect
]
