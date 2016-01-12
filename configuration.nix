# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

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
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =  with pkgs; [
     audacious
     deluge
     emacs
     encfs
     fasd
     firefox
     gcc
     ghc
     gitAndTools.gitFull
     gnupg
     gnupg1compat
     nodePackages.peerflix
     pass
     python27Packages.virtualenvwrapper
     rxvt_unicode
     thinkfan
     tmux
     tomahawk
     vimHugeX
     vlc
     wget
     wpa_supplicant
     xf86_input_mtrack
     zsh
   ];

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
    { device ="/dev/sda3";
      fsType = "ext4";
    };

  users.extraUsers.jcullen =
  { isNormalUser = true;
    home = "/home/jcullen";
    description = "Justin Cullen";
    extraGroups = [ "wheel" "networkmanager" ];
    shell = "/run/current-system/sw/bin/zsh";
  };
}
