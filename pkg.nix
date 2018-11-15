{ mkDerivation, base, stdenv, stm }:
mkDerivation {
  pname = "tstack";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base stm ];
  executableHaskellDepends = [ base stm ];
  homepage = "https://github.com/dmjio/tstack";
  description = "A concurrent, thread-safe, transactional Stack";
  license = stdenv.lib.licenses.bsd3;
}
