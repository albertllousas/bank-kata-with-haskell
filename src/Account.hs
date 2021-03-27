{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Account where

import Data.Time

type Amount = Float

data AccountEntry = Debit Amount UTCTime | Credit Amount UTCTime deriving (Eq, Show)

data Account = Account { entries:: [AccountEntry]} deriving (Eq, Show)

data Error = InvalidAmount | NotEnoughFounds deriving (Eq, Show)

--type GetAccount = AccountId -> Maybe Account

deposit ::  Amount -> UTCTime -> Account -> Either Error Account
deposit amount time account = updateAccount <$> checkAmount amount
    where updateAccount = \amount -> account { entries = entries account ++ [Credit amount time] }

withdraw :: Amount -> UTCTime -> Account -> Either Error Account
withdraw amount time account = do
                    checkAmount amount
                    checkBalance (entries account)
                    return (updateAccount account)
    where updateAccount = \account -> account { entries = entries account ++ [Debit amount time] }
          checkBalance = \entries -> if (balance entries - amount) < 0 then Left NotEnoughFounds else Right account

checkAmount :: Amount -> Either Error Amount
checkAmount amount = if amount < 0
                then Left InvalidAmount
                else Right amount

balance :: [AccountEntry] -> Amount
balance entries = foldl (+) 0 amounts
    where amounts = fmap getAmount entries
          getAmount (Debit amount _) = - amount
          getAmount (Credit amount _) = amount

bankStatement :: Account -> String
bankStatement (Account entries) = mconcat (headers : statements (reverse entries))
    where headers = "date || credit || debit || balance\n"
          statements [] = []
          statements (e:es) = (showEntry e ++ (show (balance (e:es))) ++ "\n") : statements es
          showEntry (Credit a ts) = mconcat [(formatTs ts),"||", show a, "|| ||"]
          showEntry (Debit a ts)  = mconcat [(formatTs ts), "|| ||", show a, "||"]
          formatTs ts = formatTime defaultTimeLocale "%d/%m/%Y" ts

printBankStatement :: (Account -> String) -> Either Error Account -> (String -> IO ()) -> IO ()
printBankStatement _ (Left e) printFn = printFn (show e)
printBankStatement statement (Right account) printFn = printFn (statement account)