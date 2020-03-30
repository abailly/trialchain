# Trialchain: A Dead Simple Blockchain

This is an implementation of a very simple "blockchain" aka. decentralized and distributed ledger holding a journal of monetary transactions between various opaque identities. The goal of this project is to convince myself I understand enough of blockchain to be able to emulate some of its core principles, with the notable exception of the consensus algorithm itself that powers the decentralised nature of any blockchain. This is left as an exercise for the reader!

# Instructions

## Install

Installation requires a recent version of [stack](https://haskellstack.org) and has only been tested on Mac OS X

```
$ stack install --test
```

should build everything, run all the tests and install the `trialchaind` server in `~/.local/bin` directory or whatever the isntallation path for stack is.

## Run

```
$ trialchaind
```

starts a server listening on port 8899. This port cannot be changed as there's no option to run the server.

The server logs incoming requests on its standard output in JSON format, like so:

```
{"time":"30/Mar/2020:18:41:00 +0200","response":{"status":200,"size":null,"body":null},"request":{"httpVersion":"1.1","path":"/identities","size":0,"body":"","durationMs":5.0e-2,"remoteHost":{"hostAddress":"127.0.0.1","port":53230},"headers":[["Host","localhost:8899"],["User-Agent","curl/7.64.1"],["Accept","*/*"]],"queryString":[],"method":"GET"}}
{"time":"30/Mar/2020:18:43:59 +0200","response":{"status":201,"size":null,"body":null},"request":{"httpVersion":"1.1","path":"/identities","size":133,"body":"{\"identityId\": \"48181acd22b3edaebc8a447868a7df7ce629920a\", \"key\": \"613e51e12badcdbc7899f244aec378c75a2eafd9b3d7ecc2945b55483e7efd28\"}","durationMs":6.0e-2,"remoteHost":{"hostAddress":"127.0.0.1","port":53242},"headers":[["Host","localhost:8899"],["User-Agent","curl/7.64.1"],["Accept","*/*"],["content-type","application/json"],["Content-Length","133"]],"queryString":[],"method":"POST"}}
{"time":"30/Mar/2020:18:44:04 +0200","response":{"status":409,"size":null,"body":""},"request":{"httpVersion":"1.1","path":"/identities","size":133,"body":"{\"identityId\": \"48181acd22b3edaebc8a447868a7df7ce629920a\", \"key\": \"613e51e12badcdbc7899f244aec378c75a2eafd9b3d7ecc2945b55483e7efd28\"}","durationMs":0.32,"remoteHost":{"hostAddress":"127.0.0.1","port":53244},"headers":[["Host","localhost:8899"],["User-Agent","curl/7.64.1"],["Accept","*/*"],["content-type","application/json"],["Content-Length","133"]],"queryString":[],"method":"POST"}}
{"time":"30/Mar/2020:18:45:49 +0200","response":{"status":201,"size":null,"body":null},"request":{"httpVersion":"1.1","path":"/identities","size":133,"body":"{\"identityId\": \"522b276a356bdf39013dfabea2cd43e141ecc9e8\", \"key\": \"765579e83bc7906ea9d4e68bb705d71fa5da6516292e7e34e8a64f0994d00b73\"}","durationMs":7.0e-2,"remoteHost":{"hostAddress":"127.0.0.1","port":53252},"headers":[["Host","localhost:8899"],["User-Agent","curl/7.64.1"],["Accept","*/*"],["content-type","application/json"],["Content-Length","133"]],"queryString":[],"method":"POST"}}
{"time":"30/Mar/2020:18:46:04 +0200","response":{"status":200,"size":null,"body":null},"request":{"httpVersion":"1.1","path":"/identities","size":0,"body":"","durationMs":2.0e-2,"remoteHost":{"hostAddress":"127.0.0.1","port":53256},"headers":[["Host","localhost:8899"],["User-Agent","curl/7.64.1"],["Accept","*/*"]],"queryString":[],"method":"GET"}}
{"time":"30/Mar/2020:18:46:08 +0200","response":{"status":200,"size":null,"body":null},"request":{"httpVersion":"1.1","path":"/accounts","size":0,"body":"","durationMs":2.0e-2,"remoteHost":{"hostAddress":"127.0.0.1","port":53258},"headers":[["Host","localhost:8899"],["User-Agent","curl/7.64.1"],["Accept","*/*"]],"queryString":[],"method":"GET"}}
{"time":"30/Mar/2020:18:47:12 +0200","response":{"status":200,"size":null,"body":null},"request":{"httpVersion":"1.1","path":"/transactions","size":0,"body":"","durationMs":3.0e-2,"remoteHost":{"hostAddress":"127.0.0.1","port":53261},"headers":[["Host","localhost:8899"],["User-Agent","curl/7.64.1"],["Accept","*/*"]],"queryString":[],"method":"GET"}}
```

# API

## Identities

To be able to post transactions to the ledger, one must register an identity with the server.

* `POST /identities` : Register a new `Identity` which is basically a 20-bytes unique identifier along with an Ed25519 _public key_, for example:
  ```
  {"identityId": "522b276a356bdf39013dfabea2cd43e141ecc9e8", "key": "765579e83bc7906ea9d4e68bb705d71fa5da6516292e7e34e8a64f0994d00b73"}
  ```
  Note the identity's hash and the public key are encoded as hexadecimal strings (which is not very efficient but prettier than base64).
* `GET /identities` : List all identities registered on this server
* `GET /identities/<hash>` : Retrieve a single identity given its hash

## Transactions

Transactions represent a transfer of some amount of "coin" between two registered _Accounts_. An _Account_ is basically just an `Identity`, but the server also maintains a _balance_ as part of the account, even though this balance could just be computed from balancing transactions to/from that account.

* `POST /transactions` : Attempt to post a new `Transaction` to the ledger. a transaction contains a `payload`, a reference to a  `previous` transaction hash and `signed` field containing the signature by the transaction issuer of the payload's hash plus the previous transaction's hash:
  ```
  {
  "signed": "39952bcdf2e5d69afab5e5cbb52dc1fabd2bf0462a8d70e85a4b6f4ab97726ad4f9520c00b0c74e7d750187def5294d3697615c8b710fc991cc61912d75faf01",
  "payload": {
    "amount": 1,
    "to": "48181acd22b3edaebc8a447868a7df7ce629920a",
    "from": "522b276a356bdf39013dfabea2cd43e141ecc9e8"
  },
  "previous": "0000000000000000000000000000000000000000"
  }
  ```
* `GET /transactions` : Returns a list of all transactions registered
* `GET /transactions/<hash>` : Returns a specific transactions identified by its hash
* `GET /accounts` : Returns a list of all accounts with their balances

For the sake of simplicity, when an identity is registered a "seed transaction" of 1000000000 coins is issued by the system, signed by an auto-generated private key for this server. Of course, this is just a way to get things started as normally identities would have received coins either through an initial donation or through mining or other similar operation.
