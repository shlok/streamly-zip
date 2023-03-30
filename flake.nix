{
  description = "streamly-zip";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "8107";
        packageName = "streamly-zip";

        config = {};

        overlays = [
          (final: prev:
            let
              haskellPkgs = final.haskell.packages."ghc${ghcVersion}";
            in {
              myHaskellPackages = haskellPkgs.override {
                overrides = hfinal: hprev: { 
                  ${packageName} =
                    final.haskell.lib.addBuildDepends
                      (hfinal.callCabal2nix "${packageName}" ./. {})
                      [final.libzip];
                };
              };

              ${packageName} = final.myHaskellPackages.${packageName};

              myDevShell = final.myHaskellPackages.shellFor {
                packages = p: [ p.${packageName} ];

                buildInputs = [final.libzip];

                nativeBuildInputs = [
                  haskellPkgs.cabal-install
                  haskellPkgs.haskell-language-server
                ];
              };
            })
        ];

        pkgs = import nixpkgs { inherit config overlays system; };
      in {
        packages.default = pkgs.${packageName};
        devShells.default = pkgs.myDevShell;
      });
}
