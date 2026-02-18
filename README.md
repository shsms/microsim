# Microgrid simulator

A microgrid simulator for testing applications written with the
Frequenz SDK.  It simulates batteries, inverters and EV chargers, and
also power flow between the components of the microgrid.

All the simulation logic is written in lisp, and the rust grpc server
only fetches data from the lisp side and forwards control commands to
the lisp side.

## Requirements

- A Protobuf compiler
- A recent Rust compiler

On Fedora, these can be installed with:

  ```sh
  sudo dnf install protobuf-compiler protobuf-devel rust
  ```

## How to use

The `config.lisp` file can be updated to adjust the component
configuration of the microgrid.

Initialize the submodules, then run `cargo run --release` to start the
simulator.  The `config.lisp` can be modified at runtime, to make
changes to the components.

If you have different configurations or configuration is under different
name, you can still run the binary by passing the config path as a
parameter, as long as the config files are in the same directory as the
`sim` directory.


 ```sh
 cargo run --release -- <path-to-the-config>
 ```
