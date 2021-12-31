{
  inputs.nixpkgs.url = "nixpkgs/nixos-21.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-utils.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    let
      ebus-rust-derivation = { lib, runCommandNoCC, rustPlatform, pkg-config, openssl }:
        rustPlatform.buildRustPackage rec {
          name = "ebus-rust";
          # not sure why this hack is needed to make sourceRoot work
          src = runCommandNoCC "source" { } ''
            mkdir -p $out
            cp -r ${./.}/* $out
          '';
          cargoLock = {
            lockFile = ./ebus-rust/Cargo.lock;
          };
          nativeBuildInputs = [ pkg-config ];
          buildInputs = [ openssl ];
          sourceRoot = "source/ebus-rust";
        };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        rec {
          packages = {
            ebus-rust = pkgs.callPackage ebus-rust-derivation { };
          };
          devShell = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              cargo
              rustc
              pkg-config
              rustfmt
              rls
              clippy
              bashInteractive
              nixpkgs-fmt
              gitFull
              xxd
              influxdb
              #
              racket
							#
              gnumake
              libxml2
              libxslt
              pandoc
            ];
            buildInputs = with pkgs; [
              openssl
            ];
            RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
          };
        })
    //
    {
      nixosModules = {
        ebus-rust = { config, lib, pkgs, ... }:
          let
            cfg = config.services.ebus-rust;
            ebus-rust = pkgs.callPackage ebus-rust-derivation { };
          in
          {
            options = {
              services.ebus-rust = {
                enable = lib.mkEnableOption "enable ebus parser";
                user = lib.mkOption {
                  default = "root";
                  description = "user to run the ebus process";
                };
                device = lib.mkOption {
                	default = "/dev/ttyUSB0";
                };
              };
            };

            config = lib.mkIf cfg.enable {
              systemd.services.ebus = {
                description = "ebus protocol parser and influxdb inserter";
                wantedBy = [ "multi-user.target" ];
                after = [ "networking.target" "influxdb.service" ];
                script = ''
                  ${pkgs.coreutils}/bin/stty 9600 < ${cfg.device}
                  RUST_LOG=info ${ebus-rust}/bin/ebus --enhanced influxdb < ${cfg.device}
                '';
                serviceConfig = { User = cfg.user; };
              };
            };
          };
      };
    };
}
