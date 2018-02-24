{ package ? "ghc-mod", compiler ? "ghc822" }:
let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    rev = "664cb083e756d3db1ea6b8433d3fa1d88046c6bd";
    sha256 = "0c8zycw7j6cfsydnw5r10ibhr5dcfr9iym8hc0yw1h7g9gqif7m6"; 
  };
  pkgs = import nixpkgs { config = {}; };
  inherit (pkgs) haskell;

  
  filterPredicate = p: type:
    let path = baseNameOf p; in !(
       (type == "directory" && path == "dist")
    || (type == "symlink"   && path == "result")
    || (type == "directory" && path == ".git")
    || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
    || pkgs.lib.hasSuffix "~" path
    || pkgs.lib.hasSuffix ".o" path
    || pkgs.lib.hasSuffix ".so" path
    || pkgs.lib.hasSuffix ".nix" path);
    
  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {}); 
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {}; 
         };

    {
        ghc-mod-pinned = overrideCabal (doJailbreak (cp "ghc-mod")) (drv: {
        doBenchmark = false;
        doCheck = false;
        doHaddock = false;
      
        });
        ghc-mod = overrideCabal (doJailbreak (build "ghc-mod" ./.)) (drv: {
          doBenchmark = false;
          doCheck = false;
          doHaddock = false;
        });
    };
  };
in rec {
  drv = overrides.${package};
  ghc-mod = if pkgs.lib.inNixShell then drv.env else drv;
  ghc-mod-pinned = overrides.ghc-mod-pinned;
}
