{ stdenv, pkgs, ... }:

stdenv.mkDerivation rec {
  version = "31f5d1d9dae6d408cfd6d23519f41fe8fd0824b9";
  pname = "base16-shell";
  src = pkgs.fetchFromGitHub {
    owner = "chriskempson";
    repo = pname;
    rev = version;
    sha256 = "0dbrx571d13x1zhi3zraarh563237j83v72n3l081fz0rz2rzfp0";
  };
  outputs = [ "out" ];
  installPhase = ''
mkdir -p $out
cp -r . $out
  '';
}
