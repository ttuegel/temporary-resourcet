{ mkDerivation, base, directory, exceptions, filepath, resourcet
, stdenv, tasty, tasty-hunit, transformers, unix
}:
mkDerivation {
  pname = "temporary-resourcet";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base directory exceptions filepath resourcet transformers unix
  ];
  testHaskellDepends = [
    base directory resourcet tasty tasty-hunit transformers
  ];
  homepage = "http://www.github.com/ttuegel/temporary-resourcet";
  description = "Portable temporary files and directories with automatic deletion";
  license = stdenv.lib.licenses.bsd3;
}
