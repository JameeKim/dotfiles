{
  stdenv,
  nonNixosGpu,
}:
stdenv.mkDerivation {
  name = "eos-gpu";
  meta = {
    description = "GPU driver setup for my EndaevourOS system";
    mainProgram = "eos-gpu-setup";
  };

  src = ./.;
  buildInputs = [ nonNixosGpu ];
  patchPhase = ''
    substituteInPlace eos-gpu-setup \
      --replace '@@storepath@@' '${nonNixosGpu.outPath}' \
      --replace '@@drvpath@@' '${nonNixosGpu}'
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp eos-gpu-setup $out/bin
  '';
}
