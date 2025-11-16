# SSH Configuration

This directory contains SSH configuration files that will be symlinked to `~/.ssh/`.

## Setup

1. Edit the config file to add your hosts and settings

3. Run the bootstrap symlinks task:
   ```bash
   ./bootstrap.sh symlinks
   ```

## Why Symlinks?

While SSH can read configs from `~/.ssh/`, we keep them in this repo under `~/.config/ssh/` and symlink them to:

- Track SSH configuration in version control
- Keep all configs organized under `~/.config/`
- Share configs across machines
- Maintain consistent environment setup

## Files

- **config**: Main SSH client configuration

## Security Notes

- Never commit private keys (`id_*`, `*.pem`, etc.) to version control
- The `.gitignore` excludes `*.secret` files
- Keep sensitive configs in `*.secret` files (ignored by git)
- SSH keys should remain in `~/.ssh/` (not symlinked)
- Only configuration files should be symlinked, not keys

## Removing macOS-specific Settings

If you're on Linux, remove or comment out these lines from your config:
```
  UseKeychain yes
```
