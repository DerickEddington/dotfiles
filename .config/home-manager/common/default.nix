# Common options that each user probably will not need to change.  A user's
# $XDG_CONFIG_HOME/home-manager/home.nix (which imports this file), can extend
# and/or override the options set in this file.  A user should only change this
# file, committed to the main branch of the dotfiles repository, if the change
# should be merged upstream and supplied to all users of the host system.

{ config, pkgs, lib, nixos-config, ... }:

let
  inherit (builtins) elem getEnv pathExists;
  inherit (lib) hm mkDefault mkEnableOption mkIf optionals;
  inherit (lib.attrsets) optionalAttrs;
in

let
  hostName = import ../hostName.nix;
  # (toString avoids the path coercion in antiquotation that would copy to /nix/store/.)
  systemPath = toString /run/current-system/sw;
  userProfilePath = toString ~/.nix-profile;

  early.myLib = import ../../nixpkgs/my/lib { pkgs = null; };
  inherit (early.myLib) nixosConfigLoc;

  systemWide.debuggingModule.exists =
    (    nixosConfigLoc.isDefined
      && (pathExists (nixosConfigLoc.dirName + "/debugging.nix")));
in
{
  imports = [
    ./module-args.nix
    ./devel-utils.nix
    ./emacs.nix
    ./git-svn.nix
    ./python.nix
    ./rootless-docker.nix
    ./rust.nix
    ./vagrant.nix
  ]
  ++ (optionals systemWide.debuggingModule.exists [
    ./debugging.nix
  ]);

  options.my = {
    tmpDir = mkEnableOption "`~/tmp` existence";
  };

  config = {
    #---------------------------------------------------------------------------
    # My Custom Options
    #---------------------------------------------------------------------------
    my = {
      tmpDir = mkDefault true;
    };

    systemd.user.tmpfiles.rules =
      # Also enables the systemd-tmpfiles units (cleaning (etc) of all config'ed tmpfiles.d paths)
      mkIf config.my.tmpDir ["q %h/tmp"];

    #---------------------------------------------------------------------------
    # Home Manager itself
    #---------------------------------------------------------------------------
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    home.enableNixpkgsReleaseCheck = true;

    nixpkgs.overlays = import ../../nixpkgs/my/overlays (_self: _super: {
                                debuggingSupportConfig = config.my.debugging.support;
                              });

    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    home.username = getEnv "USER";
    home.homeDirectory = getEnv "HOME";

    # Packages available in per-user profile.  Only add to this that which all users should have,
    # because it is inconvenient for them to need to remove elements from this.
    home.packages = with pkgs; [
      # Useful to each user because each user's home dir is tracked by Git.  Don't use
      # `programs.git.enable` because that would generate config files (even an empty one), but we
      # want those to not be managed by HM so they can be tracked in the dotfiles repo so they're
      # provided in non-HM-managed homes (e.g. when the dotfiles repo is used in FreeBSD, Solaris,
      # etc.).
      config.programs.git.package

      # $XDG_CONFIG_HOME/my/bash/interactive/history/ needs this `my-bash_history-combiner`
      # utility.
      (import ../../nixpkgs/my/bash_history-combiner.nix { inherit pkgs; })

      # This is sometimes helpful when doing other things that can use this (e.g. if this user's
      # home is accessed via TRAMP of Emacs).  Some users will never use this, but installing it
      # for all users doesn't hurt anything and it's tiny.
      inotify-tools
    ]
    ++ (optionals systemWide.debuggingModule.exists [
      # Exercise ../../nixpkgs/my/overlays and its addition of debugging support.
      my-hello-test
    ]);

    #---------------------------------------------------------------------------
    # Environment Variables
    #---------------------------------------------------------------------------
    home = {
      sessionVariables = rec {
        VISUAL = "emacs --no-window-system";
        EDITOR = VISUAL;
      };
    };

    #---------------------------------------------------------------------------
    # Bash
    #---------------------------------------------------------------------------
    programs.bash = {
      enable = true;

      # Added as last thing in ~/.profile, after the sessionVariables part that
      # home-manager auto-generates.
      profileExtra = ''
        . "''${XDG_CONFIG_HOME:-$HOME/.config}/my/env/profile.sh"
      '';

      # Added as first thing in ~/.bashrc, before the interactive-shell check.
      bashrcExtra = ''
      '';

      # Whatever bashrcExtra does as first thing, any following home-manager
      # options can override these same aspects of bashrcExtra if it had also
      # done these.

      # Added as last thing in ~/.bashrc, after the things home-manager
      # auto-generates, and after the interactive-shell check.  initExtra can
      # override/undo any preceding home-manager options.
      initExtra = ''
        source "''${XDG_CONFIG_HOME:-$HOME/.config}/my/bash/interactive/init.bash"
      '';
    };

    #---------------------------------------------------------------------------
    # Whether to enable GNU Info.
    #---------------------------------------------------------------------------
    programs.info.enable = true;

    #---------------------------------------------------------------------------
    # Whether to generate the manual page index caches using mandb(8). This
    # allows searching for a page or keyword using utilities like apropos(1).
    #
    # If you don't mind waiting a few more seconds when Home Manager builds a
    # new generation, you may safely enable this option.
    #---------------------------------------------------------------------------
    programs.man.generateCaches = true;

    #---------------------------------------------------------------------------
    # KeePassXC
    #---------------------------------------------------------------------------
    home.file = let
      isInstalled = pkg: (    (elem pkg nixos-config.environment.systemPackages)
                           || (elem pkg config.home.packages));
    in
      # These files are what enable web browsers with the KeePassXC-Browser extension to
      # communicate with KeePassXC.  We install these our self, instead of having KeePassXC manage
      # them, to ensure these are always present and to ensure these cannot be changed, not even
      # by KeePassXC, so that their contents as defined here are tracked in our ~/.dotfiles
      # repository.  This requires having `UpdateBinaryPath=false` in
      # ~/.config/keepassxc/keepassxc.ini, to tell KeePassXC to not manage them.  Otherwise, if
      # KeePassXC managed them, the `"path":` value would be a /nix/store/... pathname (since it
      # was compiled with such hardcoded) which would frequently change after `nixos-rebuild`s
      # which would be a hassle for tracking these in the repo; and, further, these could
      # accidentally be messed with if they weren't immutable which could break the critical
      # browser extension; and, further, any `"path":` value for one OS would be invalid when the
      # home dir is shared across multiple different OSs because the value must be absolute and
      # PATH searching isn't done.  By creating them here via Home Manager, these are only
      # installed when the OS is NixOS, where I assume that's the only OS such a home dir will be
      # used with.  For other OSs, it shouldn't be assumed how this should be handled, and the
      # stuff here won't be used.
      mkIf (isInstalled pkgs.keepassxc) ({
        # Unneeded for Firefox, since this is installed system-wide by my NixOS config, which will
        # naturally stay up-to-date automatically without the concerns mentioned above.
       #".mozilla/native-messaging-hosts/org.keepassxc.keepassxc_browser.json".text = ''
       #  {
       #      "allowed_extensions": [
       #          "keepassxc-browser@keepassxc.org"
       #      ],
       #      "description": "KeePassXC integration with native messaging support",
       #      "name": "org.keepassxc.keepassxc_browser",
       #      "path": "/run/current-system/sw/bin/keepassxc-proxy",
       #      "type": "stdio"
       #  }
       #'';
      } // (optionalAttrs (     (isInstalled pkgs.ungoogled-chromium)
                            && !(isInstalled pkgs.chromium)) {
        # Provide for Chromium, because the Nixpkgs don't have a way to have this system-wide.
        # Only provide for the Ungoogled variant, because I wouldn't trust the normal Chromium
        # with my secrets.
        ".config/chromium/NativeMessagingHosts/org.keepassxc.keepassxc_browser.json".text = ''
          {
              "allowed_origins": [
                  "chrome-extension://pdffhmdngciaglkoonimfcmckehcpafo/",
                  "chrome-extension://oboonakemofpalcgghocfoadofidjkkk/"
              ],
              "description": "KeePassXC integration with native messaging support",
              "name": "org.keepassxc.keepassxc_browser",
              "path": "/run/current-system/sw/bin/keepassxc-proxy",
              "type": "stdio"
          }
          '';
      }));

    #---------------------------------------------------------------------------
    # Firefox
    #---------------------------------------------------------------------------
    programs.firefox = {
      enable = true;

      profiles = {

        default = {

          #---------------------------------------------------------------------
          # The values below were discovered with the
          # ~/.mozilla/firefox/$PROFILE/user.js file and the about:config and
          # about:preferences pages.
          #---------------------------------------------------------------------
          settings = let
            restorePreviousSession = 3;
            compact = 1;
          in {
            "browser.uidensity" = compact;

            "browser.startup.homepage" = "about:blank";
            "browser.startup.page" = restorePreviousSession;
            "browser.sessionstore.warnOnQuit" = true;
            "browser.aboutConfig.showWarning" = false;
            "browser.download.always_ask_before_handling_new_types" = true;

            "browser.newtabpage.enabled" = false;
            "browser.newtabpage.enhanced" = false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            "browser.newtabpage.activity-stream.migrationExpired" = true;
            "browser.newtabpage.activity-stream.prerender" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = false;
            "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeVisited" = false;
            "browser.newtabpage.activity-stream.showSearch" = false;
            "browser.newtabpage.activity-stream.showSponsored" = false;
            "browser.tabs.warnOnClose" = true;

            "font.default.x-western" = "sans-serif";
            "font.size.variable.x-western" = 15;
            "font.size.monospace.x-western" = 14;
            "font.minimum-size.x-western" = 14;
            "browser.display.use_document_fonts" = 0;
            "browser.display.background_color" = "#9a9996";
            "general.smoothScroll" = true;

            "browser.search.suggest.enabled" = false;
            "browser.search.update" = false;
            "browser.search.widget.inNavBar" = true;
            "browser.urlbar.placeholderName" = "DuckDuckGo";
            "browser.urlbar.placeholderName.private" = "DuckDuckGo";
            "browser.urlbar.suggest.quicksuggest" = false;
            "browser.urlbar.suggest.quicksuggest.sponsored" = false;

            # Password management. Disabled because the KeePassXC-Browser addon is used instead.
            "signon.rememberSignons" = false;
            "signon.autofillForms" = false;
            "signon.management.page.breach-alerts.enabled" = false;  # KeePassXC can do alerts.

            # Anti-telemetry
            "toolkit.telemetry.pioneer-new-studies-available" = false;
            "app.shield.optoutstudies.enabled" = false;
            "datareporting.healthreport.uploadEnabled" = false;
            "datareporting.policy.dataSubmissionEnabled" = false;
            "signon.firefoxRelay.feature" = "disabled";
            # Do not report what I download to Mozilla's masters.
            "browser.safebrowsing.downloads.enabled" = false;
            "browser.safebrowsing.downloads.remote.block_potentially_unwanted" = false;
            "browser.safebrowsing.downloads.remote.block_uncommon" = false;

            "privacy.donottrackheader.enabled" = true;
            "privacy.globalprivacycontrol.enabled" = true;
            "privacy.globalprivacycontrol.functionality.enabled" = true;
            "privacy.globalprivacycontrol.was_ever_enabled" = true;
            "privacy.trackingprotection.enabled" = true;
            "privacy.trackingprotection.socialtracking.enabled" = true;
            "browser.contentblocking.category" = "custom";

            "extensions.formautofill.addresses.enabled" = false;
            "extensions.formautofill.addresses.usage.hasEntry" = true;
            "extensions.formautofill.creditCards.enabled" = false;
            "extensions.formautofill.firstTimeUse" = false;

            "extensions.treestyletab.autoCollapseExpandSubtreeOnAttach" = false;
            "extensions.treestyletab.autoCollapseExpandSubtreeOnSelect" = false;
            "extensions.treestyletab.insertNewChildAt" = 0;
            "extensions.treestyletab.show.context-item-reloadDescendantTabs" = true;
            "extensions.treestyletab.show.context-item-removeAllTabsButThisTree" = true;
            "extensions.treestyletab.show.context-item-removeDescendantTabs" = true;

            # Needed for Firefox to apply the userChrome.css and userContent.css
            # files (which are generated from the below).
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          };

          # Needed with the Tree Style Tab extension, to hide undesired widgets.
          userChrome = ''
            #TabsToolbar {
                visibility: collapse !important;
            }

            #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
                display: none;
            }
          '';

          # Darker background for new tabs (to not blast eyes with blinding
          # white).
          userContent = ''
            .tab:not(:hover) .closebox {
              display: none;
            }
          '';
        };
      };

      # NOTE: No longer used, because Firefox ends-up managing their updates
      #       even with this option.
      # # Note: Would be incompatible with /etc/nixos/firefox.nix having a
      # # non-empty nixExtensions list.
      # extensions = with firefox-addons; [
      #   tree-style-tab
      #   ublock-origin
      # ];
    };

    #---------------------------------------------------------------------------
    # dconf.  Affects GNOME-like desktop environments such as MATE.  The values
    # below were discovered with the CLI tool `dconf dump /`.
    #---------------------------------------------------------------------------
    dconf.settings = mkIf nixos-config.programs.dconf.enable {

      "org/mate/power-manager" = {
        action-critical-battery = "suspend";
        backlight-battery-reduce = false;
        button-lid-ac = "nothing";
        button-lid-battery = "nothing";
        button-power = "interactive";
        button-suspend = "nothing";
        icon-policy = "charge";
        kbd-backlight-battery-reduce = false;
        sleep-display-ac = 0;
        sleep-display-battery = 0;
      };

      "org/mate/desktop/session" = {
        auto-save-session = true;  # Remember running apps when logging out.
        idle-delay = 120;  # Minutes before activating screensaver.
      };

      "org/mate/screensaver" = {
        idle-activation-enabled = true;
        lock-enabled = false;
        mode = "blank-only";
      };

      "org/mate/marco/general" = {
        theme = "Green-Submarine";
        titlebar-font = "Ubuntu Medium 11";
        num-workspaces = 4;
        allow-tiling = true;
        action-double-click-titlebar = "toggle_maximize_vertically";
        show-tab-border = true;
        center-new-windows = true;
      };

      "org/mate/desktop/interface" = {
        window-scaling-factor = 1;
        font-name = "Ubuntu 13";
        document-font-name = "Ubuntu 13";
        monospace-font-name = "Ubuntu Mono 14";
      };

      "org/mate/caja/desktop" = {
        font = "Ubuntu 12";
      };

      "org/mate/desktop/sound" = {
        input-feedback-sounds = false;
        theme-name = "__no_sounds";
        event-sounds = false;
      };

      "org/mate/desktop/peripherals/mouse" = {
        cursor-size = 48;
        cursor-theme = "ComixCursors-LH-Opaque-Orange";
        drag-threshold = 8;
        motion-threshold = 10;
        motion-acceleration = 10.0;
      };

      "org/mate/desktop/peripherals/keyboard" = {
        numlock-state = "off";
      };

      "org/mate/marco/global-keybindings" = {
        run-command-terminal = "disabled";
        show-desktop = "disabled";
        switch-to-workspace-left = "<Mod4>Left";
        switch-to-workspace-right = "<Mod4>Right";
        switch-to-workspace-up = "<Mod4>Up";
        switch-to-workspace-down = "<Mod4>Down";
        switch-to-workspace-1 = "<Mod4>1";
        switch-to-workspace-2 = "<Mod4>2";
        switch-to-workspace-4 = "<Mod4>4";
        switch-to-workspace-3 = "<Mod4>3";
      };

      "org/mate/marco/window-keybindings" = {
        activate-window-menu = "disabled";
        toggle-shaded = "disabled";
        move-to-center = "disabled";
        move-to-side-w = "disabled";
        move-to-side-e = "disabled";
        move-to-side-n = "disabled";
        move-to-side-s = "disabled";
        move-to-corner-nw = "disabled";
        move-to-corner-ne = "disabled";
        move-to-corner-sw = "disabled";
        move-to-corner-se = "disabled";
        toggle-maximized = "<Alt><Mod4>Up";
        tile-to-side-w = "<Alt><Mod4>Left";
        tile-to-side-e = "<Alt><Mod4>Right";
        tile-to-corner-nw = "<Alt><Mod4>End";
        tile-to-corner-ne = "<Alt><Mod4>Home";
        tile-to-corner-sw = "<Alt><Mod4>Page_Down";
        tile-to-corner-se = "<Alt><Mod4>Page_Up";
        move-to-workspace-left = "<Primary><Alt><Mod4>Left";
        move-to-workspace-right = "<Primary><Alt><Mod4>Right";
        move-to-workspace-up = "<Primary><Alt><Mod4>Up";
        move-to-workspace-down = "<Primary><Alt><Mod4>Down";
      };

      "org/mate/terminal/profiles/default" = {
        foreground-color = "#000000000000";
        palette = "#000000000000:#828200000000:#00006E6E1010:#FCFCE9E94F4F:#100F1615C4C4:#787800008080:#000078788080:#88888A8A8585:#4D4D4D4D4D4D:#828200000000:#00006E6E1010:#FCFCE9E94F4F:#100F1615C4C4:#787800008080:#000078788080:#FFFFFFFFFFFF";
        use-system-font = false;
        silent-bell = true;
        use-theme-colors = false;
        scrollbar-position = "hidden";
        exit-action = "hold";
        default-show-menubar = false;
        font = "Ubuntu Mono 15";
        allow-bold = false;
        bold-color = "#000000000000";
        background-color = "#BDBDB8B8A0A0";
        scrollback-lines = 5000000;
      };

      "org/mate/panel/general" = {
        default-layout = "Mine";
        enable-sni-support = true;
        object-id-list = [
          "menu"
          "web-browser"
          "window-list"
          "workspace-switcher"
          "sys-load-monitor"
          "indicators"
          "clock"
        ];
        toplevel-id-list = ["bottom"];
      };

      "org/mate/panel/toplevels/bottom" = {
        expand = true;
        orientation = "bottom";
        screen = 0;
        y-bottom = 0;
        auto-hide = true;
        size = 48;
      };

      "org/mate/panel/objects/menu" = {
        locked = true;
        toplevel-id = "bottom";
        position = 0;
        object-type = "menu";
        use-menu-path = false;
        panel-right-stick = false;
        tooltip = "Main Menu";
      };

      "org/mate/panel/objects/web-browser" = {
        locked = true;
        launcher-location = "${userProfilePath}/share/applications/firefox.desktop";
        position = 48;
        object-type = "launcher";
        toplevel-id = "bottom";
        panel-right-stick = false;
      };

      "org/mate/panel/objects/music-player" = {
        locked = true;
        launcher-location = "${systemPath}/share/applications/org.gnome.Rhythmbox3.desktop";
        toplevel-id = "bottom";
        position = 96;
        object-type = "launcher";
        panel-right-stick = false;
      };

      "org/mate/panel/objects/terminal" = {
        locked = true;
        launcher-location = "${systemPath}/share/applications/mate-terminal.desktop";
        toplevel-id = "bottom";
        position = 144;
        object-type = "launcher";
        panel-right-stick = false;
      };

      "org/mate/panel/objects/source-code-editor" = {
        locked = true;
        launcher-location = "${userProfilePath}/share/applications/emacs.desktop";
        toplevel-id = "bottom";
        position = 192;
        object-type = "launcher";
        panel-right-stick = false;
      };

      "org/mate/panel/objects/window-list" = {
        applet-iid = "WnckletFactory::WindowListApplet";
        locked = true;
        toplevel-id = "bottom";
        position = 240;
        object-type = "applet";
      };

      "org/mate/panel/objects/window-list/prefs" = {
        group-windows = "auto";
      };

      "org/mate/panel/objects/workspace-switcher" = {
        applet-iid = "WnckletFactory::WorkspaceSwitcherApplet";
        locked = true;
        toplevel-id = "bottom";
        object-type = "applet";
        panel-right-stick = true;
        position = 4;
      };

      "org/mate/panel/objects/sys-load-monitor" = {
        applet-iid = "MultiLoadAppletFactory::MultiLoadApplet";
        locked = true;
        toplevel-id = "bottom";
        object-type = "applet";
        panel-right-stick = true;
        position = 3;
      };

      "org/mate/panel/objects/sys-load-monitor/prefs" = {
        size = hm.gvariant.mkUint32 70;
        speed = hm.gvariant.mkUint32 2000;
        view-memload = true;
        view-netload = true;
      };

      "org/mate/panel/objects/indicators" = {
        applet-iid = "NotificationAreaAppletFactory::NotificationArea";
        locked = true;
        object-type = "applet";
        panel-right-stick = true;
        position = 2;
        toplevel-id = "bottom";
      };

      "org/mate/panel/objects/indicators/prefs" = {
        min-icon-size = 36;
      };

      "org/mate/panel/objects/clock" = {
        applet-iid = "ClockAppletFactory::ClockApplet";
        locked = true;
        toplevel-id = "bottom";
        position = 1;
        object-type = "applet";
        panel-right-stick = true;
      };

      "org/mate/panel/objects/clock/prefs" = {
        show-temperature = false;
        show-date = false;
        expand-locations = false;
        format = "12-hour";
        show-week-numbers = false;
        custom-format = "";
        show-weather = false;
      };

      "org/mate/notification-daemon" = {
        popup-location = "bottom_right";
        theme = "coco";
      };

      "org/mate/screenshot" = {
        delay = 3;
        include-pointer = false;
        include-border = true;
        border-effect = "none";
      };

      "org/mate/caja/preferences" = {
        use-iec-units = true;
        default-folder-viewer = "list-view";
        confirm-move-to-trash = true;
        enable-delete = true;
      };
    };
  };  # config
}
