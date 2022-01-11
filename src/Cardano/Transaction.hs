module Cardano.Transaction where
import qualified Data.Map.Strict as M
import           Data.Map (Map)
import           Control.Monad.Writer
import           Control.Monad.Reader
import           System.Process
import qualified Plutus.V1.Ledger.Api as A
import qualified Cardano.Api.Shelley as S
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.List (intercalate)
import           Control.Exception
import           Text.Read
import           Control.Concurrent
import qualified Control.Lens as L
import           Data.List.Split
import qualified Data.Aeson.Lens as AL
import           System.IO.Temp
import           Data.Maybe
import           System.FilePath.Posix
import           GHC.Generics

-- TODO
-- make all the types that use json
-- polymorphic
-- they can either have value
-- or filepath
-- before converting flags they have to become filepaths

newtype Value = Value { unValue :: Map String (Map String Int) }
  deriving (Show, Eq, Ord)
type Address = String
type DatumHash = String
type TxId = String

instance Monoid Value where
  mempty = Value mempty

instance Semigroup Value where
  Value x <> Value y = Value $ M.unionWith (M.unionWith (+)) x y

diffTokenMapWithNegatives :: Map String Int -> Map String Int -> Maybe (Map String Int)
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

diffValue :: Value -> Value -> Value
diffValue (Value x) (Value y) = Value $ M.differenceWith (diffTokenMapWithNegatives) x y

pprPolicyTokens :: String -> Map String Int -> [String]
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
  , utxoDatumHash :: String
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

data TransactionBuilder = TransactionBuilder
  { tInputs        :: [Input]
  , tOutputs       :: [Output]
  , tMint          :: Value
  , tTimeRange     :: Maybe TimeRange
  , tSignatures    :: [FilePath]
  , tMetadata      :: ByteString
  , tChangeAddress :: Last Address
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
    }

newtype Tx a = Tx { unTx :: ReaderT (Maybe Integer) (WriterT TransactionBuilder IO) a }
  deriving(Functor, Applicative, Monad, MonadIO, MonadWriter TransactionBuilder, MonadReader (Maybe Integer))

getTestnetConfig :: Tx (Maybe Integer)
getTestnetConfig = ask

getTransactionBuilder :: Tx TransactionBuilder
getTransactionBuilder = snd <$> listen (pure ())

mint :: Value -> Tx ()
mint v = tell $ mempty { tMint = v }

sign :: FilePath -> Tx ()
sign x = tell $ mempty { tSignatures = [x] }

metadata :: ByteString -> Tx ()
metadata x = tell $ mempty { tMetadata = x }

timerange :: Slot -> Slot -> Tx ()
timerange start stop = tell $ mempty { tTimeRange = Just $ TimeRange start $ Just stop }

startSlot :: Slot -> Tx ()
startSlot x = tell $ mempty { tTimeRange = Just $ mempty { trStart = x } }

startNow :: Tx Slot
startNow = do
  now <- currentSlot
  tell $ mempty { tTimeRange = Just $ mempty { trStart = now } }
  pure now

ttl :: Integer -> Tx ()
ttl x = tell $ mempty { tTimeRange = Just $ mempty { trEnd = Just x } }

ttlFromNow :: Integer -> Tx ()
ttlFromNow elapsedAmount = do
  _ <- startNow
  ttl elapsedAmount

input :: UTxO -> Tx ()
input x = tell $ mempty { tInputs = [Input x Nothing] }

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
scriptInput utxo scriptFile datum redeemer = tell $ mempty {
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

parseValue' :: [String] -> Maybe Value
parseValue' xs = case xs of
  lovelacesStr:"lovelace":nonNativeTokens -> do
    lovelaces <- readMaybe lovelacesStr
    Value initialValue <- parseNonNativeTokens nonNativeTokens
    pure $ Value $ M.insert "" (M.singleton "" lovelaces) initialValue
  _ -> Nothing

-- TODO parse datum hash
parseUTxOLine :: String -> Maybe UTxO
parseUTxOLine line = case words line of
  utxoTx:utxoIndex:rest -> do
    utxoValue <- parseValue' rest
    utxoDatumHash <- parseDatumHash rest
    pure UTxO {..}
  _ -> Nothing

parseDatumHash :: [String] -> Maybe String
parseDatumHash xs = case reverse xs of
  hash:"TxOutDatumHash":"+":_ -> Just hash
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
queryUtxos address mTestnet
  = mapM (\line -> maybe (throwIO . userError $ "Failed to parse UTxO for line: " <> line) pure $ parseUTxOLine line) . drop 2 . lines
  =<< readProcess
      "cardano-cli"
      ( [ "query"
        , "utxo"
        , "--address"
        , address
        ] <>
        maybe ["--mainnet"] (\x -> ["--testnet-magic", show x]) mTestnet
      )
      ""

findScriptInputs
  :: Address
  -> DatumHash
  -> Tx [UTxO]
findScriptInputs address datumHash = do
  testnetConfig <- getTestnetConfig
  liftIO $ filter ((==datumHash) . utxoDatumHash) <$> queryUtxos address testnetConfig

hashScript :: FilePath -> IO Address
hashScript plutusFile = readFile $ replaceExtension plutusFile "addr"

-- Write datum to a temporary file
-- cardano-cli transaction hash-script-data --script-data-file
hashDatum :: Maybe Integer -> Aeson.Value -> IO String
hashDatum mTestnet value = withSystemTempFile "datum" $ \datumFile _ -> do
  BSL.writeFile datumFile $ Aeson.encode value
  readProcess
      "cardano-cli"
      ( [ "transaction"
        , "hash-script-data"
        , "--script-data-file"
        , datumFile
        ] <>
        maybe ["--mainnet"] (\x -> ["--testnet-magic", show x]) mTestnet
      )
      ""

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
  mTestnet <- ask
  scriptAddress <- liftIO $ hashScript scriptFile
  datumHash <- liftIO $ hashDatum mTestnet $ toCliJson datum
  utxo <- liftIO . maybe (throwIO $ userError "firstScriptInput: no utxos") pure . listToMaybe =<<
    findScriptInputs scriptAddress datumHash
  scriptInput utxo scriptFile datum redeemer

--
balanceNonAdaAssets :: Address
                    -- ^ Change address
                    -> Tx ()
balanceNonAdaAssets = undefined

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
  -- Merge the utxos values
  let mergeInputValue = mconcat $ map (utxoValue . iUtxo) inputs
  -- return the inputs and the remaining outputs
  pure (inputs, diffValue outputValue mergeInputValue)

selectInputsAndBalance
  :: Value
  -- ^ Outputs to match
  -> Address
  -- ^ Wallet to select inputs from
  -> Address
  -- ^ Balance address
  -> Tx ([Input], Value)
  -- ^ The inputs, change output, and the remaining unfilled outputs
selectInputsAndBalance = undefined

selectInputsSelfBalance :: Value
             -- ^ Outputs to match
             -> Address
             -- ^ Balance address
             -> Tx ([Input], Value)
             -- ^ The inputs, change output, and the remaining unfilled outputs
selectInputsSelfBalance = undefined

selectAllInputsAndSelfBalance :: Address -> Tx ([Input], Value)
selectAllInputsAndSelfBalance = undefined

currentSlotIO :: Maybe Integer -> IO Slot
currentSlotIO mTestnet = do
  either (\x -> throwIO $ userError $ "could not parse tip" <> x)
         ( maybe (throwIO $ userError "could not find slot") pure
         . L.preview (AL.key "slot" . AL._Number . L.to floor)
         )
          . (Aeson.eitherDecode :: BSL.ByteString -> Either String Aeson.Value)
          . BSLC.pack
          =<< do
    readProcess
        "cardano-cli"
        ( [ "query"
          , "tip"
          ] <>
          maybe ["--mainnet"] (\x -> ["--testnet-magic", show x]) mTestnet
        )
        ""

currentSlot :: Tx Slot
currentSlot = do
  mTestnet <- ask
  liftIO $ currentSlotIO mTestnet


output :: Address
       -> Value
       -> Tx ()
output a v = tell $ mempty { tOutputs = [Output a v Nothing] }

outputWithHash
          :: A.ToData d
          => Address
          -> Value
          -> d
          -> Tx ()
outputWithHash a v d = do
  mTestnet <- ask
  datumHash <- liftIO $ hashDatum mTestnet $ toCliJson d
  tell $
    mempty
      { tOutputs = [Output a v $ Just $ DatumInfo datumHash Nothing] }

outputWithDatum
          :: A.ToData d
          => Address
          -> Value
          -> d
          -> Tx ()
outputWithDatum a v d = do
  mTestnet <- ask
  let datumValue = toCliJson d
  datumHash <- liftIO $ hashDatum mTestnet datumValue
  tell $ mempty
    { tOutputs = [Output a v $ Just $ DatumInfo datumHash (Just datumValue) ] }

-- Get all of the utxos
-- merge the values
account :: Address -> Tx Value
account address = do
  mTestnet <- ask
  utxos <- liftIO $ queryUtxos address mTestnet
  pure $ mconcat $ map utxoValue utxos


waitForNextBlock :: Tx ()
waitForNextBlock = do
  mTestnet <- ask
  start <- currentSlot
  liftIO $ fix $ \next -> do
    threadDelay 1_000_000
    nextSlot <- currentSlotIO mTestnet
    when (start == nextSlot) next

----

toTestnetFlag :: Maybe Int -> String
toTestnetFlag = \case
  Nothing -> "--mainnet"
  Just x  -> "--testnet " <> show x

toInputFlag :: Input -> String
toInputFlag Input {iUtxo = UTxO {..}}
  =  "--tx-in "
  <> utxoTx
  <> "#"
  <> utxoIndex

inputsToFlags :: [Input] -> [String]
inputsToFlags = map toInputFlag

flattenValue :: Value -> [(String, String, Int)]
flattenValue = undefined

valueToOutput :: Value -> String
valueToOutput
  = unwords
  . concatMap
      (\(p, t, v) -> ["+", show v, p <> "." <> t])
  . flattenValue

{-

oops need to write this value out.

data DatumInfo = DatumInfo
  { diHash  :: String
  , diDatum :: Maybe Aeson.Value
  }


-}

datumToOutput :: Maybe DatumInfo -> [String]
datumToOutput = undefined
{-
datumToOutput = \case
  Nothing -> []
  Just DatumInfo {..} ->
    [
    ,
    ]
-}

outputsToFlag :: Output -> [String]
outputsToFlag Output {..}
  = [ "--tx-out "
    <> oAddress
    <> " "
    <> valueToOutput oValue
    ]
  <> datumToOutput oDatumInfo

outputsToFlags :: [Output] -> [String]
outputsToFlags = concatMap outputsToFlag

changeAddressToFlag :: Last Address -> [String]
changeAddressToFlag = undefined

signersToRequiredSignerFlags :: [FilePath] -> [String]
signersToRequiredSignerFlags = undefined

toMintFlags :: Value -> [String]
toMintFlags = undefined

toTimeRangeFlags :: Maybe TimeRange -> [String]
toTimeRangeFlags = undefined

toBodyFlags :: FilePath -> [String]
toBodyFlags = undefined

transactionBuilderToBuildFlags :: FilePath -> Maybe Int -> TransactionBuilder -> [String]
transactionBuilderToBuildFlags tmpDir testnet TransactionBuilder {..}
  =  ["transaction", "build", "--alonzo-era"]
  <> [toTestnetFlag testnet]
  <> inputsToFlags tInputs
  <> outputsToFlags tOutputs
  <> changeAddressToFlag tChangeAddress
  <> signersToRequiredSignerFlags tSignatures
  <> toMintFlags tMint
  <> toTimeRangeFlags tTimeRange
  <> toBodyFlags tmpDir

toSigningBodyFlag :: FilePath -> [String]
toSigningBodyFlag = undefined

signersToSigningFlags :: [FilePath] -> [String]
signersToSigningFlags = undefined

toSignedTxFile :: FilePath -> [String]
toSignedTxFile = undefined

transactionBuilderToSignFlags :: FilePath -> Maybe Int -> TransactionBuilder -> [String]
transactionBuilderToSignFlags tmpDir testnet TransactionBuilder {..}
  =  ["transaction", "sign"]
  <> toSigningBodyFlag tmpDir
  <> signersToSigningFlags tSignatures
  <> [toTestnetFlag testnet]
  <> toSignedTxFile tmpDir


eval :: Tx () -> IO ()
eval = undefined
