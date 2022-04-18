{ config, pkgs, lib, ... }: {
  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      sshKeys = [
        "${config.home.homeDirectory}/.ssh/id_rsa"
      ];
    };
  };
  systemd.user.services.dropbox = {
    Unit.description = "Dropbox";
    Install.WantedBy = [
      # "graphical-session.target"
      "default.target"
    ];
    Service = {
      ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
      ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };
}
