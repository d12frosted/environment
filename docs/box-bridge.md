# box-bridge

A bridge between the laptop and a remote dev box (a Coder workspace) for the
things a plain ssh session does not give:

- laptop ports that reach services on the box (a browser extension talking to
  a daemon on the box, for example);
- URLs opened on the box (`xdg-open`, `$BROWSER`) open in the laptop browser;
- OAuth logins on the box work: when the URL carries a callback on
  `localhost:<port>`, that port is forwarded before the browser opens, so the
  redirect lands on the CLI waiting on the box.

## How it works

```
laptop                                              box
------                                              ---
box-bridge run (launchd)
  ssh -M (ControlMaster) ======================== Coder agent ssh
    -L <port>:127.0.0.1:<port>     (config)  --->  127.0.0.1:<port>
    -R 127.0.0.1:<any>:<helper>              <---  box-open -> 127.0.0.1:<any>
  helper on 127.0.0.1:<helper>                     (port + token in
    `ssh -O forward -L <cb>...` per URL             ~/.local/state/box-bridge/endpoint)
    open <url>
```

- `bin/box-bridge` runs on the laptop. It starts one ssh master connection
  (`ServerAliveInterval` 10s, so a dead link is noticed within ~30s) and
  restarts it with backoff whenever it ends. After each connect it adds the
  configured forwards, asks the box for a free port to reverse-forward its
  helper to, and writes that port plus a fresh token to the endpoint file on
  the box.
- The helper listens on `127.0.0.1` only and takes `POST /open` with the
  token. It forwards every `localhost`/`127.0.0.1`/`[::1]` port mentioned in
  the URL (also percent-encoded inside `redirect_uri`, `next`, ...) to the
  same port on the box, then runs `open <url>`. Those forwards are dropped
  after 15 minutes so the laptop ports do not stay busy.
- `bin/box-open` runs on the box. eru's `coder` task links it to
  `~/.local/bin/box-open` and makes `~/.local/bin/xdg-open` and
  `coderplex-open` tiny wrappers around it. When the laptop is not connected
  it prints the URL and exits 1.

The box port is picked fresh on every connect, so a listener left behind by a
connection that died uncleanly never gets in the way.

## Setup

On the laptop, write `~/.config-private/box-bridge/config`:

```
# ssh destination of the box
host my-box
# reach the box through the Coder CLI; no ~/.ssh/config entry needed
ssh_option ProxyCommand=coder ssh --stdio my-box
ssh_option StrictHostKeyChecking=no
ssh_option UserKnownHostsFile=/dev/null
ssh_option LogLevel=ERROR

# laptop port [box port]
forward 37999
```

`host` can also be an alias from `~/.ssh/config` (say `coder.my-box` from
`coder config-ssh`); then the `ssh_option` lines are not needed. The `coder`
CLI must be logged in on the laptop.

Then load the service and check it:

```bash
~/.config/eru.sh install services
~/.config/bin/box-bridge status
tail -f ~/.local/share/box-bridge/stdout.log
```

On the box, `eru install coder` installs `box-open` and the wrappers. Point
`$BROWSER` at `~/.local/bin/box-open` where a tool reads it instead of
calling `xdg-open` (Claude Code's `env` setting, for example).

## Notes

- coderplex rewrites `~/.local/bin/xdg-open` and `coderplex-open` every time
  its daemon starts. `eru install coder` puts the wrappers back, and its
  checks warn when `xdg-open` no longer goes through `box-open`.
- `dpx auth login` picks the token-paste flow on any box with `CODER` set.
  Run it as `CODER=false dpx auth login` to get the browser flow, which then
  completes through the bridge.
- A per-URL forward fails when that port is already taken on the laptop;
  `box-open` prints the error and the URL still opens.
- Only one laptop at a time: the endpoint file on the box points at the last
  laptop that connected.
- `box-bridge` needs Python 3.9+ (the macOS Command Line Tools python is
  enough) and OpenSSH. Unit tests: `python3 -m unittest discover tests`.
