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
  libwebp
  openssh
  openssl
  python39Packages.pygments
  ripgrep
  rsync
  sqlite
  texlive.combined.scheme-full
  unrar
  unzip
  wget
] ++ lib.optionals stdenv.isDarwin [
  alacritty
  (emacs.override { nativeComp = false; })
  pinentry_mac
  skhd
  terminal-notifier
  xquartz
  yabai
  youtube-dl
] ++ lib.optionals stdenv.isLinux [
  dropbox-cli
  emacsGit
  flameshot
  rofi
  scrot
  zip
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
] ++ [
  # all things node :fear:
  nodePackages.npm
  nodePackages.pnpm
  nodePackages.typescript
  nodePackages.typescript-language-server
  nodejs
  yarn
] ++ [
  # fonts
  fontconfig
  liberation_ttf
  noto-fonts
  noto-fonts-cjk
  noto-fonts-emoji
  source-code-pro
  source-sans-pro
  source-serif-pro
  symbola
]
