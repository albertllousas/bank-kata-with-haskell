module AcceptanceSpec where

import Test.Hspec
import Test.Main
import Account
import Data.Time
import Data.ByteString.Internal
import Data.ByteString.UTF8
import System.IO.Unsafe

spec :: Spec
spec = do

  let captureStdout = (\io -> toString (prStdout (unsafePerformIO (captureProcessResult io))))

  describe "Account" $ do

    let utcTimeOf = \s -> (read (s ++ " 18:28:52.607875 UTC"))::UTCTime

    it "should print the statement after some money movements" $ do
      let account = Account []
      let updatedAccount = do
                        account' <- deposit 1000.00 (utcTimeOf "2012-01-10") account
                        account'' <- deposit 2000.00 (utcTimeOf "2012-01-13") account'
                        withdraw 500.00 (utcTimeOf "2012-01-14") account''
      let io = printBankStatement bankStatement updatedAccount putStrLn
      captureStdout io `shouldBe` "date || credit || debit || balance\n14/01/2012|| ||500.0||2500.0\n13/01/2012||2000.0|| ||3000.0\n10/01/2012||1000.0|| ||1000.0\n\n"
