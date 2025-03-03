{
  terminal = { shell = "/run/current-system/sw/bin/fish"; };
  font = { normal = { family = "Source Code Pro"; }; };
  window = {
    decorations = "none";
    option_as_alt = "both";
  };
  env = {
    # Alacritty calculates my DPI and tries to make me more
    # happy than I should be. This is heavy fix, but works
    # great.
    #
    # See https://github.com/alacritty/alacritty/issues/1501
    WINIT_X11_SCALE_FACTOR = "1.0";
  };
}
