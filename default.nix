let nixpkgs = import ./nixpkgs.nix {}; in

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskell haskellPackages lib;

  blacklist =
    [
      "\\.git"
      "\\.stack-work" "dist" "dist-newstyle"
      "\\.ghc\\.environment.*"
    ];

  isBlacklisted = baseName:
    builtins.any (black: builtins.match black baseName != null) blacklist
    ;

  filterSrc =
    let
      overrideSrc = drv: f:
        haskell.lib.overrideCabal drv (args: args // { src = f args.src; })
        ;
      predicate = path: type:
        let baseName = baseNameOf path; in
        !(isBlacklisted baseName)
        ;
    in
      drv: overrideSrc drv (src: builtins.filterSource predicate src);

  drv = haskellPackages.callPackage ./package.nix {};

in

haskell.lib.overrideCabal (filterSrc drv)
(args: args // {
  preCheck = ''
    ${args.preCheck or ""}
    export HOME="$NIX_BUILD_TOP"
    export PATH="$NIX_BUILD_TOP/$sourceRoot/dist/build/cabal2nix:$PATH"
  '';
})
