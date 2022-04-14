module Cardano.Example where
import Cardano.Transaction
import Control.Monad

buyer :: Address
buyer = "addr_test1vpmwgcd7xuqr60ej3qnlfeyy5qhaudhmnmxey6str7v4rrcd3t2q4"

buyerSKey :: FilePath
buyerSKey = "~/testnet/buyer.skey"

seller :: Address
seller = "addr_test1vq6pv6z64ldwflq5zxllfyjzxwdsgajhwg00guhrjmwq2gs0s97am"

transaction :: Tx ()
transaction = do
  Output {..} <- output seller "1000000 lovelace"
  void $ selectInputs oValue buyer

  changeAddress buyer
  void $ balanceNonAdaAssets buyer

  sign buyerSKey

test :: IO ()
test = eval (Just 1097911063) Nothing transaction
