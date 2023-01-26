# Order Book Contracts

This README will describe this package's source files in addition to each test function in the 
test suite. Refer to the parent README for an overview of the project and other information.

## Source Files

- `src/Emulator.hs`<br>
  Scenarios run with the `EmulatorTrace` monad to test application behavior. The same test
  functions are also defined in `test/Spec/Trace.hs` but are added here for convenience.

- `src/FreePolicy.hs`<br>
  A policy script which always validates successfully. Used to mint arbitrary native tokens 
  in the test suite and emulator trace scenarios.

- `src/OffChain.hs`<br>
  The off-chain component of the Plutus application. This module implements endpoints which 
  can be called by wallets interacting with the application. More specifically, the 
  `add-liquidity`, `remove-liquidity` and `trade-assets` endpoints are implemented using 
  the `Contract` monad.

- `src/OnChain.hs`<br>
  The validator script that is called when limit order UTXOs are consumed in a transaction. 

- `src/TradeNft.hs`<br>
  The policy script that is run when producing limit order UTXOs. It checks that a transaction 
  deposits correct assets to the script address governed by `src/OnChain.hs`. This policy 
  script also checks that an NFT is minted and stored in the script output being produced by 
  the transaction. This NFT is used to track a wallet's limit order(s) off-chain, and also 
  verifies that the UTXO it's stored in is valid.

- `src/OrderBook/Matching.hs`<br>
  `src/OrderBook/Model.hs`<br>
  `src/OrderBook/Utils.hs`<br>
  The order book structure and API as defined in the `mock-order-book` package. Some small 
  changes to the mock order book codebase are made to allow the API to be used with a Plutus 
  application. Refer to `mock-order-book/README.md` for more information on these modules.

## Running Test Suite and Emulator Traces

### Running test suite with `cabal run`

Clone the `plutus-apps` repository and checkout tag `v1.0.0`. The same tag is referenced in this 
project's `cabal.project` file.

```
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout v1.0.0 
```

Enter a `nix-shell` environment. Make sure to follow the setup instructions provided in
`plutus-apps` to correctly set up Nix and the IOHK hydra binaries.

```
nix-shell
```

Within `nix-shell` clone this repository somewhere on your filesystem and build the project.

```
# Step outside plutus-apps directory
cd ..

# Clone this repository and enter the order book contracts package
git clone https://github.com/danebulat/proj-order-book 
cd proj-order-book/order-book-contracts

# Build the library and test suite 
cabal build 
```

Now we can run the test suite directly with `cabal`:

```
cabal run test:order-book-tests
```

### Running test suite and emulator traces with GHCi

After building the project, its test suite can be run within GHCi by using the 
following commands:

```
cabal repl test:order-book-tests

> :l test/Spec/Trace.hs

# Run tests in IO monad 
> runTests 
```

The `test/Spec/Trace.hs` module also includes individual functions for running emulator 
traces which are called in the test suite. Seven functions are defined and named `testN` 
where `N` can be from `1` to `7`.

Use the following commands to run individual emulator trace functions:

```
cabal repl test:order-book-tests 

> :l test/Spec/Trace.hs 
> test1
  ... 
> test7
```

## Test Suite Functions

The test suite utilises trace predicates to implement unit tests. Each trace predicate 
implemented in the `test/Spec/Trace.hs` module checks wallet amounts after executing 
a particular scenario. Scenarios are implemented with the `EmulatorTrace` monad, which
can also be run via the `runEmulatorTraceIO'` function. Separate functions to run 
each emulator trace are provided in the test suite module, and will be discussed in 
this document.

### Tests Source Code

As mentioned above, the test scenarios discussed below are all defined in the 
`test/Spec/Trace.hs` module.

### Trace 1 

**Scenario:** Submit a `Buy` limit order producing 1 script output.

A wallet calls the `add-liquidity` endpoint to submit a **Buy** limit order. The limit 
order specifies that 10xAssetA will be traded for 20xAssetB. We know that 20xAssetB
are required to trade 10xAssetA because the limit price on the limit order is **2**
(Price 2 * 10xAssetA = 20xAssetB). As a result, this wallet must deposit - produce a 
UTXO at the script address with - 20xAssetB

The limit order is added to an empty order book structure. The corresponding trace predicate
for this emulator trace asserts that the wallet's funds change by minus 20xAssetB.

- `trace1`     - Emulator trace function
- `predicate1` - Trace predicate function
- `test1`      - Call to run the emulator trace in `IO` monad

### Trace 2

**Scenario:** Submit a `Sell` limit order producing 1 script output.

A wallet calls the `add-liquidity` endpoint to submit a **SELL** limit order. The limit 
order specifies that 10xAssetA will be traded for 20xAssetB. We know that 20xAssetB
are required to trade 10xAssetA because the limit price on the limit order is **2**
(Price 2 * 10xAssetA = 20xAssetB). As a result, this wallet must deposit - produce a 
UTXO at the script address with - 10xAssetA.

The limit order is added to an empty order book structure. The corresponding trace predicate
for this emulator trace asserts that the wallet's funds change by minus 10xAssetA.

- `trace2`     - Emulator trace function
- `predicate2` - Trace predicate function
- `test2`      - Call to run the emulator trace in `IO` monad

### Trace 3 

**Scenario:** Submit a `Buy` and `Sell` limit order producing 2 script outputs.

This scenario involves submitting two limit orders on either side of the market - a buy and 
a sell limit order. The resulting order book is rendered after calling this scenario's 
emulator trace function, making is possible to confirm that the bid and ask price are set 
correctly in the order book after submitting the two limit orders.

More specifically, after wallet 1 submits their buy limit order, the **bid** price should be 
set to **2**. Likewise, after wallet 2 submits their limit order, the **ask** price should be 
set to **4**.

This scenario's trace predicate asserts that the correct funds are withdrawn from the 
participating wallets correctly. Wallet 1 withdraws 20xAssetB to submit the buy order, and 
wallet 2 withdraws 10xAssetA to submit the sell order.

- `trace3`     - Emulator trace function
- `predicate3` - Trace predicate function
- `test3`      - Call to run the emulator trace in `IO` monad

### Trace 4

**Scenario:** Execute a `Buy` market order consuming 1 script output.

This scenario tests the `trade-assets` endpoint and executes a **Buy** market order. In order
to execute a market order, limit orders must already by submitted to the blockchain. With this 
in mind, wallet 1 and wallet 2 submit limit orders that are identical to the previous test.

With limit orders submitted - and therefore consumable UTXOs existing under the script address -
wallet 3 calls the `trade-assets` endpoint to buy 10xAssetA. In this case, wallet 3 will have to 
send 40xAssetB to wallet 2, whose limit order indicates that 1xAssetA will be sold for 4xAssetB.

This scenario's trade predicate asserts the correct funds are both withdrawn and received from 
the respective wallets involved.

- `trace4`     - Emulator trace function
- `predicate4` - Trace predicate function
- `test4`      - Call to run the emulator trace in `IO` monad

### Trace 5

**Scenario:** Execute a `Buy` market order consuming 2 script outputs.

This scenario builds upon trace 4 and executes a market order that has to consume two UTXOs to 
completely fill it.

More specifically, wallet 4 calls the `trade-assets` endpoint to buy 20xAssetA at the order book's
ask price. In this scenario, the liquidity (limit orders) produced by wallets 2 and 3 will be 
consumed, which will see these wallets receive AssetB in exchange for AssetA they originally
deposited in their limit orders.

This scenario's trace predicate asserts the correct funds are withdrawn and received from all
the respective wallets involved.

- `trace5`     - Emulator trace function
- `predicate5` - Trace predicate function
- `test5`      - Call to run the emulator trace in `IO` monad

### Trace 6 

**Scenario:** Execute a `Buy` limit order and cancel it, producing and consuming 1 script 
output. 

This scenario tests the `remove-liquidity` endpoint, which allows a trader to cancel their 
limit order, and get back deposited liquidity. 

Wallet 1 firstly submits a **buy** limit order by calling the `add-liquidity` endpoint. In 
total, 20xAssetB are deposited in the produced script UTXO. After some time, wallet 1 
calls the `remove-liquidity` endpoint - along with an order index to identify the UTXO 
this wallet wishes to consume. 

After cancelling the order, wallet 1 should get back the full 20xAssetB deposited earlier. 
This scenario's trace predicate assets this behavior.

- `trace6`     - Emulator trace function
- `predicate6` - Trace predicate function
- `test6`      - Call to run the emulator trace in `IO` monad

### Trace 7 

**Scenario:** Execute a `Sell` limit order and cancel it, producing and consuming 1 script 
output. 

This scenario also tests the `remove-liquidity` endpoint, this time with a sell limit order.

Wallet 1 firstly submits a **sell** limit order by calling the `add-liquidity` endpoint. In 
total, 10xAssetA are deposited in the produced script UTXO. After some time, wallet 1 
calls the `remove-liquidity` endpoint - along with an order index to identify the UTXO 
this wallet wishes to consume. 

After cancelling the order, wallet 1 should get back the full 10xAssetA deposited earlier. 
This scenario's trace predicate assets this behavior.

- `trace7`     - Emulator trace function
- `predicate7` - Trace predicate function
- `test7`      - Call to run the emulator trace in `IO` monad

### Trace 8

**Scenario:** Execute a `Buy` market order consuming 2 script outputs with different
limit prices.

This scenario involves making a trade that requires consuming two UTXOs (limit orders) at 
different price levels. In this case, multiple **sell** limit orders are submitted to 
the blockchain, each having a different price the respective trader is willing to 
swap AssetB for. 

More specifically, wallet 1 submits a limit order to sell 5xAssetA where each AssetA is 
swapped for 3xAssetB. Wallet 2 also submits a limit order to sell 5xAssetA where each 
AssetA is swapped for 4xAssetB. Wallet 1 and wallet 2 need to receive the correct amount 
of AssetB based on their submitted trade price. In addition, wallet 4 also needs to 
be able to **buy** 10xAssetA, as sufficient liquidity is available under the script 
address to make this trade.

- `trace8`     - Emulator trace function
- `predicate8` - Trace predicate function
- `test8`      - Call to run the emulator trace in `IO` monad

### Trace 9

**Scenario:** Execute a `Sell` market order consuming 2 script outputs with different
limit prices.

This scenario also involves making a trade that requires consuming two UTXOs (limit orders)
at different price levels. In this case, multiple **buy** limit orders are submitted to 
the blockchain, each having a different price the respective trader is willing to 
swap AssetA for. 

More specifically, wallet 1 submits a limit order to buy 5xAssetA where each AssetA is 
swapped for 4xAssetB. Wallet 2 also submits a limit order to buy 5xAssetA where each 
AssetA is swapped for 3xAssetB. Wallet 1 and wallet 2 need to receive the correct amount 
of AssetA based on their submitted trade price. In addition, wallet 4 also needs to 
be able to **sell** 10xAssetB, as sufficient liquidity is available under the script
address to make this trade.

- `trace9`     - Emulator trace function
- `predicate9` - Trace predicate function
- `test9`      - Call to run the emulator trace in `IO` monad

