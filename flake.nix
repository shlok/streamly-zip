{
  description = "streamly-zip";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "928";
        packageName = "streamly-zip";

        config = {};

        overlays = [
          (final: prev:
            let
              haskellPkgs = final.haskell.packages."ghc${ghcVersion}";
            in {
              myHaskellPackages = haskellPkgs.override {
                overrides = hfinal: hprev: { 
                  ${packageName} = hfinal.callCabal2nix "${packageName}" ./. {
                    streamly = hfinal.streamly_0_9_0;
                    zip = pkgs.libzip;
                  };
                };
              };

              ${packageName} = final.myHaskellPackages.${packageName};

              myDevShell = final.myHaskellPackages.shellFor {
                packages = p: [ p.${packageName} ];
                nativeBuildInputs = [
                  haskellPkgs.cabal-install
                  haskellPkgs.haskell-language-server
                ];
              };
            })
        ];

        pkgs = import nixpkgs { inherit config overlays system; };
      in {
        packages = {
          default = pkgs.${packageName};
          ${packageName} = pkgs.${packageName};
        };
        devShells.default = pkgs.myDevShell;
      });
}
