# bank-kata-with-haskell

[Bank kata](https://github.com/sandromancuso/Bank-kata) in haskell.

## Description 

Pure functional and domain kata made in haskell.

[Here](/test/AcceptanceSpec.hs), the acceptance test for the exercise.
[Here](/test/AccountSpec.hs), the unit tests.
[Here](/src/Account.hs), the prod solution.

## Run tests
```shell
stack test
```
```shell
Acceptance
  Account
    should print the statement after some money movements
Account
  Balance
    should calculate the balance for an empty list of account entries
    should calculate the balance for a list of an account entries
  Deposit
    should deposit a valid amount
    should fail depositing a negative amount
  Withdraw
    should withdraw a valid amount
    should fail withdrawing a negative amount
    should fail withdrawing when there is not enough founds
  bank statement
    should generate the bank statement for an account

Finished in 0.0025 seconds
9 examples, 0 failures
```
