{ pkgs, lib, stdenv, ... }:

with pkgs;
let exe = haskell.lib.justStaticExecutables;
in [
  aspell
  aspellDicts.en
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
  haskellPackages.haskell-language-server
  hledger
  home-manager
  jq
  killall
  kubectl
  librsvg
  libxml2
  libxslt
  nixfmt
  nixpkgs-fmt
  openssh
  openssl
  python39Packages.pygments
  ripgrep
  rsync
  shellcheck
  sqlite
  unrar
  unzip
  wget
] ++ lib.optionals stdenv.isDarwin [
  alacritty
  emacs
  skhd
  terminal-notifier
  xquartz
  yabai
  youtube-dl
] ++ lib.optionals stdenv.isLinux [
  emacsGit
]
