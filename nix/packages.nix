{ pkgs, lib, stdenv, ... }:

with pkgs;
let exe = haskell.lib.justStaticExecutables;
in [
  coreutils
  ffmpeg
  fish
  fontconfig
  gcc
  git
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
  qutebrowser
] ++ lib.optionals stdenv.isLinux [
  traceroute
  dropbox-cli
  emacsGit
  feh
  firefox
  flameshot
  pcmanfm
  rofi
  scrot
  slack
  tree
  xdg-utils
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
  file
  globalprotect-openconnect
  jetbrains.idea-ultimate
  networkmanager-openconnect
  openconnect
  patchelf
  pkg-config
  steam-run
  zoom-us
  (
    let
      python-packages-global = python-packages: with python-packages; [
        pip
        setuptools
        importlib-resources
      ];
    in python39.withPackages python-packages-global
  )
]
