# Order Book Contracts

This README will describe each test function in test suite. Refer to the parent 
README for an overview of the project and other information.

## Test Suite Functions

The test suite utilises trace predicates to implement unit tests. Each trace predicate 
implemented in the `test/Spec/Trace.hs` module checks wallet amounts after executing 
a particular scenario. Scenarios are implemented with the `EmulatorTrace` monad, which
can also be run via the `runEmulatorTraceIO'` function. Separate functions to run 
each emulator trace are provided in the test suite module, and will be discussed in 
this document.

As mentioned above, the test scenarios discussed below are all defined in the 
`test/Spec/Trace.hs` module.

### Trace 1 

A wallet calls the `add-liquidity` endpoint to submit a **BUY** limit order. The limit 
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

This scenario tests the `trade-assets` endpoint and executes a **buy** market order. In order
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


