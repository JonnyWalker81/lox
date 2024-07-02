{
  description = "Example kickstart Zig application project.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    zig.url = "github:mitchellh/zig-overlay";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          inherit (pkgs) dockerTools stdenv zig zls nodejs valgrind rr lldb;
          inherit (dockerTools) buildImage;
          name = "jlox";
          version = "0.1.0";
          overlays = [
            inputs.zig.overlays.default
            (final: prev: {
              # ... things you need to patch ...
              # zig = inputs.zig-overlay;
              zigpkgs = inputs.zig.packages.${prev.system};
              zig = inputs.zig.packages.${prev.system}."0.13.0";
            })
          ];
        in {
          packages = {
            default = stdenv.mkDerivation {
              inherit name;
              inherit version;
              src = ./.;
              nativeBuildInputs = [ zig.hook zig zls nodejs valgrind rr lldb ];
              shellHook = ''
                export ZIG_GLOBAL_CACHE_DIR=$PWD/zig-out
              '';
            };
            docker = buildImage {
              inherit name;
              tag = version;
              config = {
                Cmd = [ "${self'.packages.default}/bin/${name}" ];
                Env = [
                  "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                ];
              };
            };
          };
        };
    };
}
