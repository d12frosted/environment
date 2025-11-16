# GnuPG Configuration

This directory contains GnuPG configuration files that will be symlinked to `~/.gnupg/`.

## Setup

1. Edit the files to customize for your system:
   - **gpg-agent.conf**: Update `pinentry-program` path for your OS
     - macOS: `/opt/homebrew/bin/pinentry-mac`
     - Linux: `/usr/bin/pinentry-curses` or `/usr/bin/pinentry-gtk-2`

2. Run the bootstrap symlinks task:
   ```bash
   ./bootstrap.sh symlinks
   ```

3. Reload gpg-agent to apply changes:
   ```bash
   gpg-connect-agent reloadagent /bye
   ```

## Why Symlinks?

GnuPG doesn't natively support the XDG Base Directory specification and expects configs in `~/.gnupg/`. By keeping configs in this repo under `~/.config/gnupg/` and symlinking them, we can:

- Track GnuPG configuration in version control
- Keep all configs organized under `~/.config/`
- Share configs across machines
- Maintain consistent environment setup

## Files

- **gpg-agent.conf**: Agent configuration (pinentry, cache settings, SSH support)
- **gpg.conf**: Main GPG configuration (key preferences, cipher settings)
- **dirmngr.conf**: Directory manager configuration (keyservers, proxies)

## Security Notes

- Never commit private keys to version control
- The `.gitignore` excludes `*.secret` and `*.gpg` files
- Keep sensitive configs in `*.secret` files (ignored by git)
- Permissions will be automatically fixed to 600/700 by the bootstrap script
