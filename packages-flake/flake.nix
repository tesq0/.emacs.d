{
  description = "Emacs pacakge dependencies";
  
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  inputs.yasnippet = {
    type = "github";
    owner = "joaotavora";
    repo = "yasnippet";
    rev = "3bf9a3b1af37174a004798b7195826af0123fa6a";
    flake = false;
  };

  inputs.ace-window = {
    type = "github";
    owner = "abo-abo";
    repo = "ace-window";
    rev = "7003c88cd9cad58dc35c7cd13ebc61c355fb5be7";
    flake = false;
  };

  inputs.avy = {
    type = "github";
    owner = "abo-abo";
    repo = "avy";
    rev = "f2cf43b5372a6e2a7c101496c47caaf03338de36";
    flake = false;
  };

  inputs.php-mode = {
    type = "github";
    owner = "emacs-php";
    repo = "php-mode";
    rev = "2440330456e6b73ba3725d1bebddfe67cb6e6e5c";
    flake = false;
  };
  
  inputs.rg = {
    type = "github";
    owner = "dajva";
    repo = "rg.el";
    rev = "444a8ccfea0b38452a0bc4c390a8ee01cfe30017";
    flake = false;
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    nix,
    ...
  } @args: 
    flake-utils.lib.eachDefaultSystem (system:
    with import nixpkgs { inherit system; };
    {
      packages = {
	default = derivation {
	  name = "emacs-packages";
	  builder = "${bash}/bin/bash";
	  args = [ ./build-packages.sh ];
	  deps = map (x: [x args."${x}"]) ["yasnippet" "ace-window" "avy" "php-mode" "rg"];
   	  inherit coreutils system;
        };
      };
   });
}