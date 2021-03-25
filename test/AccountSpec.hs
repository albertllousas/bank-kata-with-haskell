module AccountSpec where

import Test.Hspec
import Account
import Data.Time

spec :: Spec
spec = do

  let time = (read "2011-11-19 18:28:52.607875 UTC")::UTCTime

  let emptyAccount = Account { entries = [] }

  describe "Balance" $ do

    it "should calculate the balance for an empty list of account entries" $ do
      balance [] `shouldBe` 0.00

    it "should calculate the balance for a list of an account entries" $ do
      balance [Credit 100.00 time, Debit 50.00 time] `shouldBe` 50.00

  describe "Deposit" $ do

    it "should deposit a valid amount" $ do
      deposit 100.00 time emptyAccount `shouldBe` Right (Account { entries = [Credit 100.00 time] })

    it "should fail depositing a negative amount" $ do
      deposit (-100.00) time emptyAccount `shouldBe` Left InvalidAmount

  describe "Withdraw" $ do

    let time = (read "2011-11-19 18:28:52.607875 UTC")::UTCTime
    let account = Account { entries = [Credit 100.00 time] }

    it "should withdraw a valid amount" $ do
      withdraw 50.00 time account `shouldBe` Right (Account { entries = [Credit 100.00 time, Debit 50.00 time] })

    it "should fail withdrawing a negative amount" $ do
      deposit (-100.00) time account `shouldBe` Left InvalidAmount

    it "should fail withdrawing when there is not enough founds" $ do
      withdraw 200.00 time account `shouldBe` Left NotEnoughFounds

  describe "Bank Statement" $ do

    it "should generate the bank statement for an account" $ do
      let account = Account [Credit 100.00 time, Debit 50.00 time]
      bankStatement account `shouldBe` "date || credit || debit || balance\n19/11/2011|| ||50.0||50.0\n19/11/2011||100.0|| ||100.0\n"
