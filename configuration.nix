# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_4_3;

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "nodev";
  };

  security.pam.enableEcryptfs = true;

  networking = {
    hostName = "nixos-jcullen";
    networkmanager.enable = true;
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  nixpkgs.config = {
  vimHugeX.python = true;
  allowUnfree = true;

  firefox = {
    enableGoogleTalkPlugin = true;
    enableAdobeFlash = true;
  };

  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =  with pkgs; [

     alsaUtils
     audacious
     chromium
     clang
     cmake
     curl
     deluge
     dropbox
     dropbox-cli
     emacs
     encfs
     fasd
     ffmpeg
     firefox
     gcc
     ghc
     gitAndTools.gitFull
     gnumake
     gnupg
     gnupg1compat
     htop
     nix-repl
     nodejs
     nodePackages.peerflix
     pass
     pcmanfm
     python27Full
     python27Packages.ipython
     python27Packages.pip
     python27Packages.virtualenv
     python27Packages.virtualenvwrapper
     rsync
     rxvt_unicode
     screen
     silver-searcher
     slim
     sshfsFuse
     # texLiveFull
     thinkfan
     tmux
     tomahawk
     unzip
     valgrind
     vimHugeX
     vlc
     w3m
     wget
     wpa_supplicant
     xf86_input_mtrack
     xmonad-with-packages
     xorg.xf86inputsynaptics
     xorg.xmodmap
     zathura
     zsh

   ];

    environment.interactiveShellInit = ''
    # A nix query helper function
    nq()
    {
      case "$@" in
        -h|--help|"")
          printf "nq: A tiny nix-env wrapper to search for packages in package name, attribute name and description fields\n";
          printf "\nUsage: nq <case insensitive regexp>\n";
          return;;
      esac
      nix-env -qaP --description \* | grep -i "$@"
    }
    export HISTCONTROL=ignoreboth   # ignorespace + ignoredups
    export HISTSIZE=1000000         # big big history
    export HISTFILESIZE=$HISTSIZE
    shopt -s histappend             # append to history, don't overwrite it
    '';

   sound = {
     enableMediaKeys = true;
   };

   # hardware configuration
   hardware = {
     trackpoint = {
       enable = true;
       emulateWheel = true;
     };
   };

   # configure services
   services = {
     # Enable the OpenSSH daemon.
     openssh.enable = true;

     # Enable CUPS to print documents.
     printing.enable = true;

     # thinkfan.enable = true;

     # X11 stuff
     xserver = {
       enable = true;
       layout = "us";
       xkbOptions = "eurosign:e";
       windowManager.default = "xmonad";
       windowManager.xmonad = {
         enableContribAndExtras = true;
         enable = true;
       };
       desktopManager.kde4.enable = true;
       synaptics = {
         enable = true;
         twoFingerScroll = true;
       };
     };
   };


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

  # Mount /home partition.
  fileSystems."/home" =
    { device ="/dev/sda7";
      fsType = "btrfs";
    };

  # Mount /data partition.
  fileSystems."/data" =
    { device ="/dev/sda5";
      fsType = "ext4";
    };

  # Mount /data partition.
  fileSystems."/boot" =
    { device ="/dev/sda6";
      fsType = "vfat";
    };

  users.extraUsers.jcullen =
  { isNormalUser = true;
    home = "/home/jcullen";
    description = "Justin Cullen";
    extraGroups = [ "wheel" "networkmanager" ];
    shell = "/run/current-system/sw/bin/zsh";
  };
}
