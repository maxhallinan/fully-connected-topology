# fully-connected-topology

Launch a toy network with a fully connected topology.
Input to stdin is broadcast to the network.
Messages from the network are printed to stdout.

## Example

```
fully-connected-topology 127.0.0.1:3000 '127.0.0.1:3001,127.0.0.1:3002'
```

## Usage

```
Usage: fully-connected-topology OWN_ADDRESS PEER_ADDRESSES

Available options:
  OWN_ADDRESS              the local address of the client
  PEER_ADDRESSES           a comma-separated list of peer addresses
  -h,--help                Show this help text
```
