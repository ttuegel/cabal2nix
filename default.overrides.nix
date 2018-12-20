self: super:
let lib = self.haskell.lib; in
{
  haskellPackages = super.haskellPackages.override (args: {
    overrides = self: super:
      (args.overrides or (self: super: super)) self super // {
        hpack = self.callPackage ./hpack.nix {};
        yaml = self.callPackage ./yaml.nix {};
      };
  });
}
