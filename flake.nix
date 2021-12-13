{
  description = "Write your GDB scripts in Haskell";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=master";
    fu.url = "github:numtide/flake-utils?ref=master";
    hls.url = "github:haskell/haskell-language-server?ref=master";
  };

  outputs = { self, np, fu, hls }:
    with fu.lib;
    with np.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        version =
          "${substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
        config = { };
        overlay = final: _:
          with final;
          with haskell.lib;
          with haskellPackages.extend (final: _: { }); {
            debugger-hs = (callCabal2nix "debugger-hs" ./. { }).overrideAttrs
              (prev: { version = "${prev.version}.${version}"; });
          };
        overlays = [ hls.overlay overlay ];
      in with (import np { inherit system config overlays; }); rec {
        inherit overlays;
        packages = flattenTree (recurseIntoAttrs { inherit debugger-hs; });
        defaultPackage = packages.debugger-hs;
        devShell = with haskellPackages;
          shellFor {
            packages = _: [ ];
            buildInputs = [ cabal-install ghc ghcid haskell-language-server ];
          };
      });
}
