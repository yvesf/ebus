# eBus

- [ebus-racket](ebus-racket/) eBus protocol parser written in racket
- [ebus-rust](ebus-rust/) eBus protocol parser writtin in rust
- [ebus-xml](ebus-xml/) eBus protocol specification (devices, packets, fields) in xml.

## Usage of ebus-rust on nixos

```
    inputs.ebus.url = "github:yvesf/ebus";
    inputs.ebus.inputs.nixpkgs.follows = "template/nixpkgs";
    ...
    # add module
    ebus.nixosModules.ebus-rust
    ...
    # configure module
    services.ebus-rust.enable = true;
    services.ebus-rust.user = settings.username;
    services.ebus-rust.device = "/dev/ttyUSB0";
    ...
```