# User-specific options.  Can override or extend those defined by
# ./home/common.nix, or can add additional ones.  Changes to this file should
# only be committed to your per-user branch of the dotfiles repository.
#
# See: https://nix-community.github.io/home-manager/options.html

{ config, pkgs, ... }:

with builtins;

{
  imports = [ ./home/common.nix ];

  # Packages available in per-user profile.
  home.packages = with pkgs; [
  ];

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

    # extensions = with pkgs.nur.repos.rycee.firefox-addons; [
    #   user-agent-string-switcher
    #   stylus
    # ];
  };

  # Extend the imported options.
  dconf.settings = with pkgs.lib; {
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

  # TODO: Maybe this should change to 21.11 for when I install that new release
  # on new laptop.
  #
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
