{ config, pkgs, lib, ... }: {
  home.file.".xsession" = {
    executable = true;
    text = ''
export PATH=/nix/var/nix/profiles/default/bin:$PATH
export PATH=/run/current-system/sw/bin:$PATH
export PATH=$HOME/.nix-profile/bin:$PATH
export PATH=/run/wrappers/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.config/bin:$PATH

export XDG_CONFIG_HOME=${config.xdg.configHome}

userresources=$XDG_CONFIG_HOME/xorg/xresources
usermodmap=$XDG_CONFIG_HOME/xorg/xmodmap

if [ -f "$userresources" ]; then
  xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
  xmodmap "$usermodmap"
fi

# I need this so much outside of Emacs.
${pkgs.xorg.xset}/bin/xset r rate 180 26

# Make sure that Caps doesn't miss its purpose
setxkbmap -option caps:ctrl_modifier

# I rarely see my wallpapers. But when I do, I am happy.
fehbg &

# Because every X needs some algebra to tame it.
dbus-launch d12-xmonad --replace
    '';
  };
}
