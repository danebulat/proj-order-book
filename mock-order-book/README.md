# Mock Order Book

## Overview

A mock order book structure and API. The goal of this package is to simulate how 
a basic order book behaves and derive meaningful conclusions on how to implement a
decentralised application using the Cardano blockchain's eUTxO model with an 
order book system.

The code is not production ready and is simply a rapid prototype to:
- Implement an order book structure along with an API to manipulate it.
- Understand the necessary operations that must be considered when developing 
  a decentralised application utilising an order book.
- Derive conclusions about the possible architecture of a decentralised application 
  running on the Cardano blockchain utilising the eUTxO model.

## Source Files

- `src/OrderBook/Model.hs`<br>
  Data types defining the order book structure along with `Show` and `Default` 
  instances of the order book type.

- `src/OrderBook/Matching.hs`<br>
  Functions that simulate executing a market order. When a market order is executed,
  orders are "taken" from the given order book instance to fill that market order at
  the current bid or ask price. Functions exist for simulating both buy and sell 
  market orders.

- `src/OrderBook/Utils.hs`<br>
  Utilities that query, modify and work with an order book structure. 

## Test Suite

To understand how the mock order book structure and its API work, consult the test 
suite. Tests are separated into two files:

- `test/Spec/BasicOrderBookTests.hs`<br>
  Tests that assert the basic API and behaviour of the order book structure is 
  correct. 

- `test/Spec/LargeOrderBookTests.hs`<br>
  An order book storing many limit orders is used in each test in this module 
  to confirm that market orders are executed as expected.

The whole test suite can be run by calling the `main` function in `test/Spec.hs`. 
Consult the following section on how to run the test suite.

## Running the Test Suite

The test suite can be run using cabal from the package's root directory:

```
cd mock-order-book
cabal run mock-order-book:test:tests
```

Alternatively, the `test/Spec.hs` module's `main` function can be called within 
GHCi to run the entire test suite:

```
cd mock-order-book
cabal repl mock-order-book-tests:tests 
> main
```

