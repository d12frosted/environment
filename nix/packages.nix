{ pkgs, lib, stdenv, ... }:

with pkgs;
let exe = haskell.lib.justStaticExecutables;
in [
  coreutils
  ffmpeg
  fish
  fontconfig
  git
  git-filter-repo
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
  ncdu
  openssh
  openssl
  pandoc
  python39Packages.pygments
  ripgrep
  rsync
  sqlite
  texlive.combined.scheme-full
  unrar
  unzip
  wget
] ++ lib.optionals stdenv.isDarwin [
  (emacs.override { nativeComp = false; })
  pinentry_mac
  skhd
  terminal-notifier
  xquartz
  yabai
  youtube-dl
  qutebrowser
] ++ lib.optionals stdenv.isLinux [
  traceroute
  dropbox-cli
  emacsGit
  feh
  firefox
  flameshot
  pcmanfm
  scrot
  slack
  tree
  xdg-utils
  zip
  pinta
  krita
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
  font-awesome_4
  fontconfig
  liberation_ttf
  noto-fonts
  noto-fonts-cjk
  noto-fonts-emoji
  source-code-pro
  source-sans-pro
  source-serif-pro
  symbola
] ++ lib.optionals stdenv.isLinux [
  # work
  globalprotect-openconnect
  jetbrains.idea-ultimate
  networkmanager-openconnect
  openconnect
]
