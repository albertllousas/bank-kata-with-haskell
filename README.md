# bank-kata-with-haskell

[Bank kata](https://github.com/sandromancuso/Bank-kata) in haskell.

## The problem to solve
```
Given a client makes a deposit of 1000 on 10-01-2012
And a deposit of 2000 on 13-01-2012
And a withdrawal of 500 on 14-01-2012
When she prints her bank statement
Then she would see
date || credit || debit || balance
14/01/2012 || || 500.00 || 2500.00
13/01/2012 || 2000.00 || || 3000.00
10/01/2012 || 1000.00 || || 1000.00
```

## The solution

[Here](/test/AcceptanceSpec.hs), the acceptance test for the exercise.
```haskell
it "should print the statement after some money movements" $ do
      let account = Account []
      let updatedAccount = do
                        account' <- deposit 1000.00 (utcTimeOf "2012-01-10") account
                        account'' <- deposit 2000.00 (utcTimeOf "2012-01-13") account'
                        withdraw 500.00 (utcTimeOf "2012-01-14") account''
      let io = printBankStatement bankStatement updatedAccount putStrLn
      captureStdout io `shouldBe` "date || credit || debit || balance\n14/01/2012|| ||500.0||2500.0\n13/01/2012||2000.0|| ||3000.0\n10/01/2012||1000.0|| ||1000.0\n\n"
```

[Here](/test/AccountSpec.hs), the unit tests.

[Here](/src/Account.hs), the solution.

### Design notes

This is a pure domain kata, it means that any architectural pattern has been used, the solution has been made focussing
in the domain. If we would like to use it in a real app, we would need to apply a layered architecture, ports-and-adapters 
or any other on top of the current solution to have a proper separation of concerns.

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
  Bank Statement
    should generate the bank statement for an account

Finished in 0.0025 seconds
9 examples, 0 failures
```
