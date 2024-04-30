# User-specific options.  Can override or extend those defined by ./common,
# or can add additional ones.  Changes to this file should only be committed to
# your per-user branch of the dotfiles repository.
#
# See: https://nix-community.github.io/home-manager/options.html

{ config, pkgs, lib, ... }:

let
  inherit (builtins) pathExists;
  inherit (lib) mkForce optional;
in

let
  hostName = import ./hostName.nix;
  # nur = config.my.nur.fetched;
  inherit (config.my.rycee.fetched) firefox-addons;
in
{
  imports = [
    ./common
  ]
  ++ (let perHostName = ./per-host + "/${hostName}";
      in optional (pathExists perHostName) perHostName);

  # Packages available in per-user profile.
  home.packages = with pkgs; [
  ];

  my.emacs.extraPackages = [(epkgs:
    with epkgs; [
    ]
  )];

  # my.rust = {
  #   enable = true;
  #   toolchains = {
  #   };
  # };

  # my.vagrant.enable = true;

  # # Have debug-info and source-code for packages where this is applied.  This is for packages
  # # that normally don't provide these, and this uses my custom approach that overrides and
  # # overlays packages to achieve having these.
  # my.debugging.support.all.enable = true;

  # Extend the imported options.
  programs.firefox = {
    # profiles = {
    #   default = {
    #     settings = let
    #       playDRMcontent = "media.eme.enabled";
    #     in {
    #       ${playDRMcontent} = true;
    #     };
    #   };
    # };

    # # Note: Would be incompatible with /etc/nixos/firefox.nix having a non-empty
    # # nixExtensions list.
    # extensions = with firefox-addons; [
    #   user-agent-string-switcher
    #   stylus
    # ];
  };

  # Extend the imported options.
  dconf.settings = {
    # # Different mouse theme.
    # "org/mate/desktop/peripherals/mouse" = {
    #   cursor-theme = mkForce "ComixCursors-Opaque-Green";
    # };
    # # More launchers in panel than ./home/common.nix has by default.
    # "org/mate/panel/general" = {
    #   object-id-list = mkForce [
    #     "menu"
    #     "web-browser"
    #     "music-player"
    #     "terminal"
    #     "source-code-editor"
    #     "window-list"
    #     "workspace-switcher"
    #     "sys-load-monitor"
    #     "indicators"
    #     "clock"
    #   ];
    # };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
