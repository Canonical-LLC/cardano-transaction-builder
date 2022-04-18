{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Transaction where

import qualified Data.Map.Strict as M
import           Data.Map (Map)
import           Control.Monad.Managed
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Monoid
import qualified Plutus.V1.Ledger.Api as A
import qualified Cardano.Api.Shelley as S
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Function (on)
import           Data.List (intercalate, maximumBy)
import           Data.List.Extra (trim)
import           Control.Exception
import           Text.Read (readMaybe)
import           Control.Concurrent
import qualified Control.Lens as L
import           Data.List.Split
import qualified Data.Aeson.Lens as AL
import           System.IO.Temp
import           Data.Maybe
import           System.FilePath.Posix
import           GHC.Generics
import           Data.String
import           System.IO
import           System.Exit
import           System.Process.Typed

newtype Value = Value { unValue :: Map String (Map String Integer) }
  deriving (Show, Eq, Ord)
type Address = String
type DatumHash = String
type TxId = String

instance IsString Value where
  fromString
    = fromMaybe (error "FromString: failed to parse Value")
    . parseValue

instance Monoid Value where
  mempty = Value mempty

instance Semigroup Value where
  Value x <> Value y = Value $ M.unionWith (M.unionWith (+)) x y

data EvalException = EvalException String [String] String
  deriving Show

instance Exception EvalException

diffTokenMap :: Map String Integer -> Map String Integer -> Maybe (Map String Integer)
diffTokenMap x y =
  let
    diffCoin a b =
         let a' = a - b
         in if a' < 1
              then Nothing
              else Just a'

    new = M.differenceWith diffCoin x y

    in if new == mempty
          then Nothing
          else Just new

diffValues :: Value -> Value -> Value
diffValues (Value x) (Value y) = Value $ M.differenceWith (diffTokenMap) x y

diffTokenMapWithNegatives :: Map String Integer -> Map String Integer -> Maybe (Map String Integer)
diffTokenMapWithNegatives x y =
  let
    diffCoin a b =
      let a' = a - b
      in if a' == 0
        then Nothing
        else Just a'

    new = M.differenceWith diffCoin x y

    in if new == mempty
          then Nothing
          else Just new

diffValuesWithNegatives :: Value -> Value -> Value
diffValuesWithNegatives (Value x) (Value y) = Value $ M.differenceWith (diffTokenMapWithNegatives) x y

pprPolicyTokens :: String -> Map String Integer -> [String]
pprPolicyTokens policyId tokenMap = if policyId == ""
  then map (\count -> show count <> " lovelace") $ M.elems tokenMap
  else map (\(tokenName, count) -> show count <> " " <> policyId <> "." <> tokenName )
    $ M.toList tokenMap

pprValue :: Value -> String
pprValue
  = intercalate " + "
  . concatMap (uncurry pprPolicyTokens)
  . M.toList
  . unValue

data ScriptInfo = ScriptInfo
  { siScript    :: FilePath
  , siDatum     :: Aeson.Value
  , siRedeemer  :: Aeson.Value
  } deriving (Show, Eq, Ord, Generic)

data DatumInfo = DatumInfo
  { diHash  :: String
  , diDatum :: Maybe Aeson.Value
  } deriving (Show, Eq, Ord, Generic)

data UTxO = UTxO
  { utxoIndex     :: String
  , utxoTx        :: TxId
  , utxoDatumHash :: Maybe String
  , utxoValue     :: Value
  } deriving (Show, Eq, Ord)

data Input = Input
  { iUtxo       :: UTxO
  , iScriptInfo :: Maybe ScriptInfo
  } deriving (Show, Eq, Ord, Generic)


inputFromUTxO :: UTxO -> Input
inputFromUTxO x = Input x Nothing

data Output = Output
  { oAddress   :: Address
  , oValue     :: Value
  , oDatumInfo :: Maybe DatumInfo
  } deriving (Show, Eq, Ord, Generic)

type Slot = Integer

data TimeRange = TimeRange
  { trStart :: Slot
  , trEnd   :: Maybe Integer
  } deriving (Show, Eq, Ord)

instance Monoid TimeRange where
  mempty = TimeRange 0 Nothing

instance Semigroup TimeRange where
  a <> b = TimeRange
    { trStart = min (trStart a) (trStart b)
    , trEnd   = case (trEnd a, trEnd b) of
        (Nothing, Nothing) -> Nothing
        (Just  x, Nothing) -> Just x
        (Nothing, Just  y) -> Just y
        (Just  x, Just  y) -> Just $ max x y
    }

data Mint = Mint
  { mValue :: Value
  , mScript :: FilePath
  , mRedeemer :: Aeson.Value
  } deriving (Show, Eq, Ord, Generic)

data TransactionBuilder = TransactionBuilder
  { tInputs        :: [Input]
  , tOutputs       :: [Output]
  , tMint          :: [Mint]
  , tTimeRange     :: Maybe TimeRange
  , tSignatures    :: [FilePath]
  , tMetadata      :: ByteString
  , tChangeAddress :: Last Address
  , tCollateral    :: Last UTxO
  } deriving (Show, Eq, Ord, Generic)

instance Semigroup TransactionBuilder where
  x <> y = TransactionBuilder
    { tInputs        = tInputs     x <> tInputs     y
    , tOutputs       = tOutputs    x <> tOutputs    y
    , tMint          = tMint       x <> tMint       y
    , tTimeRange     = tTimeRange  x <> tTimeRange  y
    , tSignatures    = tSignatures x <> tSignatures y
    , tMetadata      = tMetadata   x <> tMetadata   y
    , tChangeAddress = tChangeAddress  x <> tChangeAddress    y
    , tCollateral    = tCollateral x <> tCollateral y
    }

instance Monoid TransactionBuilder where
  mempty = TransactionBuilder
    { tInputs        = mempty
    , tOutputs       = mempty
    , tMint          = mempty
    , tTimeRange     = mempty
    , tSignatures    = mempty
    , tMetadata      = mempty
    , tChangeAddress = mempty
    , tCollateral    = mempty
    }

newtype Tx a = Tx { unTx :: ReaderT (Maybe Integer) (StateT TransactionBuilder IO) a }
  deriving(Functor, Applicative, Monad, MonadIO, MonadState TransactionBuilder, MonadReader (Maybe Integer))

getTestnetConfig :: Tx (Maybe Integer)
getTestnetConfig = ask

putpend :: TransactionBuilder -> Tx ()
putpend tb = modify (<> tb)

getTransactionBuilder :: Tx TransactionBuilder
getTransactionBuilder = get

mint :: Aeson.ToJSON a => Value -> FilePath -> a -> Tx ()
mint v s r = putpend $ mempty { tMint = pure . Mint v s . Aeson.toJSON $ r}

sign :: FilePath -> Tx ()
sign x = putpend $ mempty { tSignatures = [x] }

metadata :: ByteString -> Tx ()
metadata x = putpend $ mempty { tMetadata = x }

timerange :: Slot -> Slot -> Tx ()
timerange start stop = putpend $ mempty { tTimeRange = Just $ TimeRange start $ Just stop }

startSlot :: Slot -> Tx ()
startSlot x = putpend $ mempty { tTimeRange = Just $ mempty { trStart = x } }

startNow :: Tx Slot
startNow = do
  now <- currentSlot
  putpend $ mempty { tTimeRange = Just $ mempty { trStart = now } }
  pure now

ttl :: Integer -> Tx ()
ttl x = putpend $ mempty { tTimeRange = Just $ mempty { trEnd = Just x } }

ttlFromNow :: Integer -> Tx ()
ttlFromNow elapsedAmount = do
  _ <- startNow
  ttl elapsedAmount

changeAddress :: Address -> Tx ()
changeAddress addr = putpend $ mempty { tChangeAddress = pure addr }

collateral :: UTxO -> Tx ()
collateral utxo = putpend $ mempty { tCollateral = pure utxo }

input :: UTxO -> Tx ()
input x = putpend $ mempty { tInputs = [Input x Nothing] }

scriptInput
  :: (A.ToData d, A.ToData r)
  => UTxO
  -- ^ Script UTxO
  -> FilePath
  -- ^ Script File
  -> d
  -- ^ Datum
  -> r
  -- ^ Redeemer
  -> Tx ()
scriptInput utxo scriptFile datum redeemer = putpend $ mempty {
    tInputs = pure $ Input utxo $ Just $ ScriptInfo
      { siDatum     = toCliJson datum
      , siRedeemer  = toCliJson redeemer
      , siScript    = scriptFile
      }
  }

toCliJson :: A.ToData a => a -> Aeson.Value
toCliJson
  = S.scriptDataToJson S.ScriptDataJsonDetailedSchema
  . S.fromPlutusData
  . A.toData

parseValue :: String -> Maybe Value
parseValue = parseValue' . words

parseValue' :: [String] -> Maybe Value
parseValue' xs = case xs of
  lovelacesStr:"lovelace":nonNativeTokens -> do
    lovelaces <- readMaybe lovelacesStr
    Value initialValue <- parseNonNativeTokens nonNativeTokens
    pure $ Value $ M.insert "" (M.singleton "" lovelaces) initialValue
  _ -> Nothing

parseUTxOLine :: String -> Maybe UTxO
parseUTxOLine line = case words line of
  utxoTx:utxoIndex:rest -> do
    utxoValue <- parseValue' rest
    let utxoDatumHash = parseDatumHash rest
    pure UTxO {..}
  _ -> Nothing

parseDatumHash :: [String] -> Maybe String
parseDatumHash xs = case reverse xs of
  hash:_:"TxOutDatumHash":"+":_ -> Just . takeWhile (/= '"') . drop 1 $ hash
  _ -> Nothing

parseNonNativeTokens :: [String] -> Maybe Value
parseNonNativeTokens = go mempty where
  go (Value acc) xs = case xs of
    [] -> Just $ Value acc
    "+":"TxOutDatumNone":[] -> Just $ Value acc
    "+":"TxOutDatumHash":_ -> Just $ Value acc
    "+":countStr:asset:rest -> do
      count <- readMaybe countStr
      (policyId, tokenName) <- case splitOn "." asset of
        [policyId, tokenName] -> Just (policyId, tokenName)
        _ -> Nothing

      let newAcc = Value $ M.insertWith (<>) policyId (M.singleton tokenName count) acc

      go newAcc rest
    _ -> Nothing

queryUtxos :: Address -> Maybe Integer -> IO [UTxO]
queryUtxos address mTestnet =
  let
    p = proc "cardano-cli" $
      [ "query"
      , "utxo"
      , "--address"
      , address
      ] <>
      maybe ["--mainnet"] (\x -> ["--testnet-magic", show x]) mTestnet

    parse = mapM (\line -> maybe (throwIO . userError $ "Failed to parse UTxO for line: " <> line) pure $ parseUTxOLine line) . drop 2 . lines . BSLC.unpack
  in
    parse =<< readProcessStdout_ p

findScriptInputs
  :: Address
  -> DatumHash
  -> Tx [UTxO]
findScriptInputs address datumHash = do
  testnetConfig <- getTestnetConfig
  liftIO $ filter ((==Just datumHash) . utxoDatumHash) <$> queryUtxos address testnetConfig

hashScript :: FilePath -> IO Address
hashScript plutusFile = readFile $ replaceExtension plutusFile "addr"

-- Write datum to a temporary file
-- cardano-cli transaction hash-script-data --script-data-file
-- TODO use the
hashDatum :: Aeson.Value -> IO String
hashDatum value = withSystemTempFile "datum" $ \datumFile fh -> do
  BSL.hPutStr fh $ Aeson.encode value
  hClose fh
  fmap (trim . BSLC.unpack) . readProcessStdout_ . proc "cardano-cli" $
    [ "transaction"
    , "hash-script-data"
    , "--script-data-file"
    , datumFile
    ]

firstScriptInput
  :: (A.ToData d, A.ToData r)
  => FilePath
  -- ^ Script File
  -> d
  -- ^ Datum
  -> r
  -- ^ Redeemer
  -> Tx ()
firstScriptInput scriptFile datum redeemer = do
  scriptAddress <- liftIO $ hashScript scriptFile
  datumHash <- liftIO $ hashDatum $ toCliJson datum
  utxo <- liftIO . maybe (throwIO $ userError "firstScriptInput: no utxos") pure . listToMaybe =<<
    findScriptInputs scriptAddress datumHash
  scriptInput utxo scriptFile datum redeemer

splitNonAdaAssets :: Value -> (Value, Value)
splitNonAdaAssets (Value m)
  = ( Value $ maybe mempty (M.singleton "") $ M.lookup "" m
    , Value $ M.delete "" m
    )

-- Look up the input.
-- Merge the inputs.
-- Merge the outputs.
-- diff the inputs from the outputs.
balanceNonAdaAssets :: Address
                    -- ^ Change address
                    -> Tx (Maybe Output)
balanceNonAdaAssets addr = do
  TransactionBuilder {..} <- getTransactionBuilder
  let
    inputValue = mconcat $ map (utxoValue . iUtxo) tInputs
    outputValue = mconcat $ map oValue tOutputs
    theDiffValue = inputValue `diffValues` outputValue

    -- Make sure there are non-ada assets in there
    (Value ada, Value nonAda) = splitNonAdaAssets theDiffValue
  if nonAda == mempty then pure Nothing else do
    -- add them with the minimum Ada
    let
      adaAmount = fromMaybe 0
                $ M.lookup ""
                $ fromMaybe mempty
                $ M.lookup "" ada

      minAdaAmount = min (3_000_000) adaAmount
      withExtraAda = Value $ M.insert "" (M.singleton "" minAdaAmount) nonAda

    Just <$> output addr withExtraAda



selectInputs :: Value
             -- ^ Outputs to match
             -> Address
             -- ^ Wallet to select inputs from
             -> Tx ([Input], Value)
             -- ^ The inputs and the remaining unfilled outputs
selectInputs outputValue address = do
  -- lookup inputs for the address
  testnetConfig <- ask
  inputs <- map inputFromUTxO <$> liftIO (queryUtxos address testnetConfig)

  putpend $ mempty { tInputs = inputs }
  -- Merge the utxos values
  let mergeInputValue = mconcat $ map (utxoValue . iUtxo) inputs
  -- return the inputs and the remaining outputs
  pure (inputs, diffValuesWithNegatives outputValue mergeInputValue)

-- Okay so this finds all of the inputs that can
-- cover the outputs. Then it balances the left over
-- to the balance address.
selectInputsAndBalance
  :: Value
  -- ^ Outputs to match
  -> Address
  -- ^ Wallet to select inputs from
  -> Address
  -- ^ Balance address
  -> Tx ([Input], Value)
  -- ^ The inputs, change output, and the remaining unfilled outputs
selectInputsAndBalance outputValue addr balanceAddr = do
  --
  (inputs, remaining) <- selectInputs outputValue addr
  let
    covered = outputValue `diffValues` remaining
    combinedInput = mconcat $ map (utxoValue . iUtxo) inputs
    change = combinedInput `diffValues` covered

  _ <- output balanceAddr change
  pure (inputs, remaining)

-- Same as above but self balances
selectInputsSelfBalance :: Value
             -- ^ Outputs to match
             -> Address
             -- ^ Balance address
             -> Tx ([Input], Value)
             -- ^ The inputs, change output, and the remaining unfilled outputs
selectInputsSelfBalance o a = selectInputsAndBalance o a a

-- Select for all the inputs and self balance
selectAllInputsAndSelfBalance :: Address -> Tx ([Input], Value)
selectAllInputsAndSelfBalance addr = do
  TransactionBuilder {..} <- getTransactionBuilder
  let combinedOutput = mconcat $ map oValue tOutputs
  selectInputsSelfBalance combinedOutput addr

-- Select an input to use as collateral
selectCollateralInput :: Address -> Tx (Input, Value)
selectCollateralInput addr = do
  -- lookup inputs for the address
  testnetConfig <- ask
  inputs <- map inputFromUTxO <$> liftIO (queryUtxos addr testnetConfig)
  let lovelaces :: Input -> Integer
      lovelaces = fromMaybe 0 . M.lookup "" . fromMaybe mempty . M.lookup "" . unValue . utxoValue . iUtxo
  let i@Input {..} = maximumBy (compare `on` lovelaces) inputs

  collateral iUtxo

  pure (i, utxoValue iUtxo)



currentSlotIO :: Maybe Integer -> IO Slot
currentSlotIO mTestnet = do
  either (\x -> throwIO $ userError $ "could not parse tip" <> x)
         ( maybe (throwIO $ userError "could not find slot") pure
         . L.preview (AL.key "slot" . AL._Number . L.to floor)
         )
          . (Aeson.eitherDecode :: BSL.ByteString -> Either String Aeson.Value)
          =<< do
    readProcessStdout_ . proc "cardano-cli" $
      [ "query"
      , "tip"
      ] <>
      maybe ["--mainnet"] (\x -> ["--testnet-magic", show x]) mTestnet


currentSlot :: Tx Slot
currentSlot = do
  mTestnet <- ask
  liftIO $ currentSlotIO mTestnet


output :: Address
       -> Value
       -> Tx Output
output a v = do
  let out = Output a v Nothing
  putpend $ mempty { tOutputs = [out] }
  pure out

outputWithHash
          :: A.ToData d
          => Address
          -> Value
          -> d
          -> Tx ()
outputWithHash a v d = do
  datumHash <- liftIO $ hashDatum $ toCliJson d
  putpend $
    mempty
      { tOutputs = [Output a v $ Just $ DatumInfo datumHash Nothing] }

outputWithDatum
          :: A.ToData d
          => Address
          -> Value
          -> d
          -> Tx ()
outputWithDatum a v d = do
  let datumValue = toCliJson d
  datumHash <- liftIO $ hashDatum datumValue
  putpend $ mempty
    { tOutputs = [Output a v $ Just $ DatumInfo datumHash (Just datumValue) ] }

-- Get all of the utxos
-- merge the values
account :: Address -> Tx Value
account address = do
  mTestnet <- ask
  utxos <- liftIO $ queryUtxos address mTestnet
  pure $ mconcat $ map utxoValue utxos


waitForNextBlock :: Maybe Integer -> IO ()
waitForNextBlock mTestnet = do
  start <- currentSlotIO mTestnet
  putStrLn . mconcat $ [ "start slot is: ", show start ]
  liftIO $ fix $ \next -> do
    putStrLn "waiting 1s"
    threadDelay 1_000_000
    nextSlot <- currentSlotIO mTestnet
    putStrLn . mconcat $ [ "current slot is: ", show nextSlot ]
    when (start == nextSlot) next

----

toTestnetFlags :: Maybe Integer -> [String]
toTestnetFlags = \case
  Nothing -> ["--mainnet"]
  Just x  -> ["--testnet-magic", show x]

managedSystemTempFile :: String -> Managed (FilePath, Handle)
managedSystemTempFile n = managed (withSystemTempFile n . curry)

toScriptFlags :: ScriptInfo -> Managed [String]
toScriptFlags ScriptInfo{..} = do
  (datumFile, dfh) <-  managedSystemTempFile "datum.json"
  liftIO $ do
    BSL.hPutStr dfh . Aeson.encode $ siDatum
    hClose dfh

  (redeemerFile, rfh) <- managedSystemTempFile "redeemer.json"
  liftIO $ do
    BSL.hPutStr rfh . Aeson.encode $ siRedeemer
    hClose rfh

  pure
    [ "--tx-in-script-file"
    , siScript
    , "--tx-in-datum-file"
    , datumFile
    , "--tx-in-redeemer-file"
    , redeemerFile
    ]

toInputFlags :: Input -> Managed [String]
toInputFlags Input {..} =
  mappend ["--tx-in", pprUtxo iUtxo] <$> maybe (pure []) toScriptFlags iScriptInfo

pprUtxo :: UTxO -> String
pprUtxo UTxO{..} = utxoTx <> "#" <> utxoIndex

inputsToFlags :: [Input] -> Managed [String]
inputsToFlags = fmap mconcat . traverse toInputFlags

flattenValue :: Value -> [(String, String, Integer)]
flattenValue (Value m) =  concatMap (\(pId, t) -> map (\(tn, c) -> (pId, tn, c)) $ M.toList t) $ M.toList m

valueToOutput :: Value -> String
valueToOutput
  = unwords
  . concatMap
      (\(p, t, v) -> ["+", show v, if p == "" then "lovelace" else p <> "." <> t])
  . flattenValue

pprJson :: Aeson.Value -> String
pprJson = BSLC.unpack . Aeson.encode

datumToOutputs :: Maybe DatumInfo -> [String]
datumToOutputs = \case
  Nothing -> []
  Just DatumInfo {..}
    -> ["--tx-out-datum-hash", diHash]
       ++ case diDatum of
          Nothing -> []
          Just d -> ["--tx-out-datum-embed-value", pprJson d]


outputToFlags :: Output -> [String]
outputToFlags Output {..}
  | oValue == mempty = []
  | otherwise
    = [ "--tx-out"
      , oAddress <> " " <> valueToOutput oValue
      ]
    <> datumToOutputs oDatumInfo

outputsToFlags :: [Output] -> [String]
outputsToFlags = concatMap outputToFlags

changeAddressToFlag :: Last Address -> [String]
changeAddressToFlag = \case
  Last Nothing -> error "Missing change address!"
  Last (Just a) -> ["--change-address", a]

collateralToFlags :: Last UTxO -> [String]
collateralToFlags = \case
  Last Nothing -> []
  Last (Just utxo) -> [ "--tx-in-collateral", pprUtxo utxo]

signersToRequiredSignerFlags :: [FilePath] -> [String]
signersToRequiredSignerFlags = concatMap (("--required-signer":) . (:[]))

toMintFlags :: Mint -> [String]
toMintFlags Mint{..}
  | mValue == mempty = []
  | otherwise =
    [ "--mint"
    , pprValue mValue
    , "--minting-script-file"
    , mScript
    , "--mint-redeemer-value"
    , pprJson mRedeemer
    ]

mintsToFlags :: [Mint] -> [String]
mintsToFlags = concatMap toMintFlags

toTimeRangeFlags :: Maybe TimeRange -> [String]
toTimeRangeFlags = \case
  Nothing -> []
  Just TimeRange {..}
    -> ["--invalid-before", show trStart]
    ++ case trEnd of
        Nothing -> []
        Just e -> ["--invalid-hereafter", show e]

toProtocolParams :: Maybe FilePath -> [String]
toProtocolParams = maybe [] (("--protocol-params-file":) . pure)

toBodyFlags :: FilePath -> [String]
toBodyFlags tmpDir = ["--out-file", tmpDir </> "body.txt"]

transactionBuilderToBuildFlags :: FilePath -> Maybe Integer -> Maybe FilePath -> TransactionBuilder -> Managed [String]
transactionBuilderToBuildFlags tmpDir testnet protocolParams TransactionBuilder {..} = do
  inputs <- inputsToFlags tInputs
  pure . mconcat $
    [ ["transaction", "build", "--alonzo-era"]
    , toTestnetFlags testnet
    , toProtocolParams protocolParams
    , inputs
    , collateralToFlags tCollateral
    , outputsToFlags tOutputs
    , changeAddressToFlag tChangeAddress
    , signersToRequiredSignerFlags tSignatures
    , mintsToFlags tMint
    , toTimeRangeFlags tTimeRange
    , toBodyFlags tmpDir
    ]

toSigningBodyFlags :: FilePath -> [String]
toSigningBodyFlags tmpDir = ["--tx-body-file", tmpDir </> "body.txt"]

signersToSigningFlags :: [FilePath] -> [String]
signersToSigningFlags = concatMap (("--signing-key-file":) . (:[]))

toSignedTxFiles :: FilePath -> [String]
toSignedTxFiles tmpDir = ["--out-file", tmpDir </> "signed-body.txt"]

transactionBuilderToSignFlags :: FilePath -> Maybe Integer -> TransactionBuilder -> [String]
transactionBuilderToSignFlags tmpDir testnet TransactionBuilder {..} = mconcat
  [ ["transaction", "sign"]
  , toSigningBodyFlags tmpDir
  , signersToSigningFlags tSignatures
  , toTestnetFlags testnet
  , toSignedTxFiles tmpDir
  ]

eval :: Maybe Integer -> Maybe FilePath -> Tx () -> IO ()
eval mTestnet protocolParams (Tx m) =
  let
    runCardanoCli args = do
      (exitCode, outStr) <- readProcessInterleaved . proc "cardano-cli" $ args
      case exitCode of
        ExitSuccess -> pure ()
        ExitFailure _ -> liftIO . throwIO . EvalException "cardano-cli" args . BSLC.unpack $ outStr

  in runManaged $ do
    tempDir <- managed (withSystemTempDirectory "tx-builder")
    txBuilder <- liftIO . execStateT (runReaderT m mTestnet) $ mempty
    bodyFlags <- transactionBuilderToBuildFlags tempDir mTestnet protocolParams txBuilder

    liftIO $ do
      runCardanoCli bodyFlags

      runCardanoCli . transactionBuilderToSignFlags tempDir mTestnet $ txBuilder

      runCardanoCli . mconcat $
        [ [ "transaction", "submit" ]
        , toTestnetFlags mTestnet
        , ["--tx-file", tempDir </> "signed-body.txt"]
        ]
