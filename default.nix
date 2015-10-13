{ stdenv, haskellngPackages }:

	let env = haskellngPackages.ghcWithPackages (p: with p; [
		aeson
		async
		bytestring
		containers
		hslogger
		mtl
		parsec
		random
		scotty
		stm
		text
		wai-extra
		warp
		safe
		cabal-install
	]);
  	in
	stdenv.mkDerivation {
		name        = "project-name";
		buildInputs = [env];
		shellHook   = ''
			export NIX_GHC="${env}/bin/ghc"
			export NIX_GHCPKG="${env}/bin/ghc-pkg"
			export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
			export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
		'';
	}
