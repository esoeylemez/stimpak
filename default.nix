{ mkDerivation, base, dependent-sum, kan-extensions, semigroupoids
, stdenv, stm, these
}:
mkDerivation {
  pname = "stimpak";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base dependent-sum kan-extensions semigroupoids stm these
  ];
  homepage = "https://github.com/esoeylemez/stimpak";
  description = "Functional reactive programming library";
  license = stdenv.lib.licenses.bsd3;
}
