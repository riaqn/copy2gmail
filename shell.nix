with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  ghc = haskell.packages.ghc7103.ghc;
  name = "myEnv";
  buildInputs = [ ncurses ];
}
