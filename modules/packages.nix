{ pkgs, lib, stdenv, ... }:

with pkgs;
let exe = haskell.lib.justStaticExecutables;
in [
  alacritty
  aspell
  aspellDicts.en
  base16-shell
  coreutils
  emacs
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
  ripgrep
  rsync
  shellcheck
  sqlite
  unrar
  unzip
  wget
] ++ lib.optionals stdenv.isDarwin [
  skhd
  terminal-notifier
  xquartz
  yabai
  youtube-dl
]
