{ pkgs, lib, stdenv, ... }:

with pkgs;
let exe = haskell.lib.justStaticExecutables;
in [
  coreutils
  ffmpeg
  fish
  fontconfig
  git-lfs
  gnugrep
  gnumake
  gnupg
  gnuplot
  gnused
  gnutar
  hledger
  home-manager
  hpack
  jq
  killall
  kubectl
  openssh
  openssl
  python39Packages.pygments
  ripgrep
  rsync
  source-code-pro
  sqlite
  unrar
  unzip
  wget
] ++ lib.optionals stdenv.isDarwin [
  alacritty
  emacs
  pinentry_mac
  skhd
  terminal-notifier
  xquartz
  yabai
  youtube-dl
] ++ lib.optionals stdenv.isLinux [
  emacsGit
] ++ [
  # all things editor
  (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
  nixfmt
  nixpkgs-fmt
  ormolu
  shellcheck
] ++ [
  # all things haskell
  cabal2nix
  hpack
]
