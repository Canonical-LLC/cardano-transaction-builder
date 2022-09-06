{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import qualified Data.Aeson.Lens as AL
import           System.IO.Temp
import           Data.Maybe
import           System.FilePath.Posix
import           GHC.Generics
import           Data.String
import           System.IO
import           System.Exit
import           System.Process.Typed
-- import           Control.Applicative
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char
import           Data.Bifunctor

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
  then map (\theCount -> show theCount <> " lovelace") $ M.elems tokenMap
  else map (\(tokenName, theCount) -> show theCount <> " " <> policyId <> "." <> tokenName )
    $ M.toList tokenMap

pprValue :: Value -> String
pprValue
  = intercalate " + "
  . concatMap (uncurry pprPolicyTokens)
  . M.toList
  . unValue

data InputDatum
  = Datum Aeson.Value
  | InlineDatum
  deriving (Show, Eq, Ord, Generic)

data InplaceScriptInfo = InplaceScriptInfo
  { isiScript   :: FilePath
  , isiDatum    :: InputDatum
  , isiRedeemer :: Aeson.Value
  } deriving (Show, Eq, Ord, Generic)

data SpendingReferenceInfo = SpendingReferenceInfo
  { srIsPlutusV2     :: Bool
  , srReferenceInput :: UTxO
  , srDatum          :: InputDatum
  , srRedeemer       :: Aeson.Value
  } deriving (Show, Eq, Ord, Generic)

data UTxODatum
  = UTxO_NoDatum
  | UTxO_DatumHash String
  | UTxO_InlineDatum (Maybe Aeson.Value)
  deriving (Show, Eq, Ord, Generic)

data UTxO = UTxO
  { utxoIndex  :: Integer
  , utxoTx     :: TxId
  , utxoValue  :: Value
  , utxoDatum  :: UTxODatum
  } deriving (Show, Eq, Ord, Generic)

data ScriptInfo
  = NoScript
  | SimpleScriptReference UTxO
  | SpendingReference SpendingReferenceInfo
  | InplaceScript InplaceScriptInfo
  deriving (Show, Eq, Ord, Generic)

data Input = Input
  { iUtxo       :: UTxO
  , iScriptInfo :: ScriptInfo
  } deriving (Show, Eq, Ord, Generic)

data ReadonlyInput = ReadonlyInput
  { roiUtxo :: UTxO
  } deriving (Show, Eq, Ord, Generic)

inputFromUTxO :: UTxO -> Input
inputFromUTxO x = Input x NoScript

data OutputDatum
  = NoOutputDatum
  | OutputDatumHash String
  | OutputDatumValue Aeson.Value
  | OutputDatumInlineValue Aeson.Value
  deriving (Show, Eq, Ord, Generic)

data Output = Output
  { oAddress         :: Address
  , oValue           :: Value
  , oDatumInfo       :: OutputDatum
  , oScriptReference :: Maybe FilePath
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
  { tInputs         :: [Input]
  , tReadonlyInputs :: [ReadonlyInput]
  , tOutputs        :: [Output]
  , tMint           :: [Mint]
  , tTimeRange      :: Maybe TimeRange
  , tSignatures     :: [FilePath]
  , tMetadata       :: ByteString
  , tChangeAddress  :: Last Address
  , tCollateral     :: Last UTxO
  } deriving (Show, Eq, Ord, Generic)

instance Semigroup TransactionBuilder where
  x <> y = TransactionBuilder
    { tInputs         = tInputs         x <> tInputs         y
    , tReadonlyInputs = tReadonlyInputs x <> tReadonlyInputs y
    , tOutputs        = tOutputs        x <> tOutputs        y
    , tMint           = tMint           x <> tMint           y
    , tTimeRange      = tTimeRange      x <> tTimeRange      y
    , tSignatures     = tSignatures     x <> tSignatures     y
    , tMetadata       = tMetadata       x <> tMetadata       y
    , tChangeAddress  = tChangeAddress  x <> tChangeAddress  y
    , tCollateral     = tCollateral     x <> tCollateral     y
    }

instance Monoid TransactionBuilder where
  mempty = TransactionBuilder
    { tInputs         = mempty
    , tReadonlyInputs = mempty
    , tOutputs        = mempty
    , tMint           = mempty
    , tTimeRange      = mempty
    , tSignatures     = mempty
    , tMetadata       = mempty
    , tChangeAddress  = mempty
    , tCollateral     = mempty
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
input x = putpend $ mempty { tInputs = [Input x NoScript] }

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
    tInputs = pure $ Input utxo $ InplaceScript $ InplaceScriptInfo
      { isiDatum     = Datum $ toCliJson datum
      , isiRedeemer  = toCliJson redeemer
      , isiScript    = scriptFile
      }
  }

scriptReferenceV2Input
  :: (A.ToData d, A.ToData r)
  => UTxO
  -- ^ UTxO to spend
  -> UTxO
  -- ^ Script UTxO
  -> d
  -- ^ Datum
  -> r
  -- ^ Redeemer
  -> Tx ()
scriptReferenceV2Input utxo scriptReferenceUtxo datum redeemer = putpend $ mempty {
    tInputs = pure $ Input utxo $ SpendingReference $ SpendingReferenceInfo
      { srIsPlutusV2 = True
      , srReferenceInput = scriptReferenceUtxo
      , srDatum     = Datum $ toCliJson datum
      , srRedeemer       = toCliJson redeemer
      }
  }

toCliJson :: A.ToData a => a -> Aeson.Value
toCliJson
  = S.scriptDataToJson S.ScriptDataJsonDetailedSchema
  . S.fromPlutusData
  . A.toData

parseValue :: String -> Maybe Value
parseValue = parseMaybe parseValue'

parseValue' :: Parser Value
parseValue' = do
  lovelaces <- L.signed space L.decimal
  space1
  void $ string "lovelace"
  mTokens <- fmap unValue <$> optional parseNonNativeTokens
  pure $ Value $ case mTokens of
    Just theTokens -> M.insert "" (M.singleton "" lovelaces) theTokens
    Nothing -> M.singleton "" (M.singleton "" lovelaces)

type Parser = Parsec String String


parseEmtpyState :: String -> Parser a -> Either String a
parseEmtpyState line theParser = bimap show id $ parse theParser "" line

parseTxId :: Parser String
parseTxId = some hexDigitChar

parseUTxOIndex :: Parser Integer
parseUTxOIndex = L.signed space L.decimal

-- TODO can't just split by words
-- Need to take the first word and the second etc
parseUTxOLine :: String -> Either String UTxO
parseUTxOLine line = parseEmtpyState line $ do
  utxoTx    <- parseTxId
  space1
  utxoIndex <- parseUTxOIndex
  space1
  utxoValue <- parseValue'
  space1
  utxoDatum <- parseDatum
  pure UTxO {..}

deriving instance Read S.ScriptData

scriptDataStringToJson :: String -> Maybe Aeson.Value
scriptDataStringToJson str = do
  scriptData <- readMaybe str :: Maybe S.ScriptData
  pure $ S.scriptDataToJson S.ScriptDataJsonDetailedSchema scriptData

parseDatum :: Parser UTxODatum
parseDatum = do
  void $ string "+"
  space1
  (UTxO_DatumHash   <$> parseDatumHash)
    <|> (UTxO_InlineDatum . Just <$> parseInlineDatum )
    <|> (UTxO_NoDatum <$ string "TxOutDatumNone" )


parseDatumHash :: Parser String
parseDatumHash = do
  void $ string "TxOutDatumHash"
  (space1 *> string "ScriptDataInBabbageEra" *> space1) <|> space1
  void $ char '"'
  hash <- some hexDigitChar
  void $ char '"'
  pure hash


parseInlineDatum :: Parser Aeson.Value
parseInlineDatum = do
  void $ string "TxOutDatumInline"
  space1
  void $ string "ReferenceTxInsScriptsInlineDatumsInBabbageEra"
  space1
  datumValueString <- many anySingle
  maybe mzero pure $ scriptDataStringToJson datumValueString

parseCountPolicyIdTokenName :: Parser (Integer, String, String)
parseCountPolicyIdTokenName = do
  space1
  void $ string "+"
  space1
  theCount <- L.signed space L.decimal
  space1
  policyId <- some hexDigitChar
  void $ char '.'
  tokenName <- some hexDigitChar
  pure (theCount, policyId, tokenName)


parseNonNativeTokens :: Parser Value
parseNonNativeTokens = do
  countsAndAssets <- some (Text.Megaparsec.try parseCountPolicyIdTokenName)

  pure $ Value $ foldr (\(theCount, policyId, tokenName) acc -> M.insertWith (<>) policyId (M.singleton tokenName theCount) acc) mempty countsAndAssets


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

    parseAction = mapM (\line -> either (\msg -> throwIO . userError $ "Failed to parse UTxO for line: " <> line <> " msg: " <> show msg) pure $ parseUTxOLine line) . drop 2 . lines . BSLC.unpack
  in
    parseAction =<< readProcessStdout_ p

findScriptInputs
  :: Address
  -> UTxODatum
  -> Tx [UTxO]
findScriptInputs address datum = do
  testnetConfig <- getTestnetConfig
  liftIO $ filter ((== datum) . utxoDatum) <$> queryUtxos address testnetConfig

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
    findScriptInputs scriptAddress (UTxO_DatumHash datumHash)
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
  let out = Output a v NoOutputDatum Nothing
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
      { tOutputs = [Output a v (OutputDatumHash datumHash) Nothing] }

outputWithDatum
          :: A.ToData d
          => Address
          -> Value
          -> d
          -> Tx ()
outputWithDatum a v d = do
  let datumValue = toCliJson d
  putpend $ mempty
    { tOutputs = [Output a v (OutputDatumValue datumValue) Nothing] }

outputWithInlineDatum
          :: A.ToData d
          => Address
          -> Value
          -> d
          -> Tx ()
outputWithInlineDatum a v d = do
  let datumValue = toCliJson d
  putpend $ mempty
    { tOutputs = [Output a v (OutputDatumInlineValue datumValue) Nothing] }

outputWithScriptReference
  :: Address
  -> Value
  -> FilePath
  -> Tx Output
outputWithScriptReference a v fp = do
  let out = Output a v NoOutputDatum $ Just fp
  putpend $ mempty { tOutputs = [out] }
  pure out

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
toScriptFlags = \case
  NoScript -> pure []
  SimpleScriptReference utxo -> pure ["--simple-script-tx-in-reference", pprUtxo utxo]
  SpendingReference SpendingReferenceInfo {..} -> do
    let plutusV2Flags = if srIsPlutusV2
          then ["--spending-plutus-script-v2"]
          else []

    (redeemerFile, rfh) <- managedSystemTempFile "redeemer.json"
    liftIO $ do
      BSL.hPutStr rfh . Aeson.encode $ srRedeemer
      hClose rfh

    datumFlags <- case srDatum of
      Datum x -> do
        (datumFile, dfh) <- managedSystemTempFile "datum.json"
        liftIO $ do
          BSL.hPutStr dfh $ Aeson.encode x
          hClose dfh

        pure ["--spending-reference-tx-in-datum-file", datumFile]

      InlineDatum -> pure ["--spending-reference-tx-in-inline-datum-present"]

    pure
      $ [ "--spending-tx-in-reference"
        , pprUtxo srReferenceInput
        ]
      <> plutusV2Flags
      <> datumFlags
      <> [ "--spending-reference-tx-in-redeemer-file"
         , redeemerFile
         ]
  InplaceScript InplaceScriptInfo {..} -> do
    (redeemerFile, rfh) <- managedSystemTempFile "redeemer.json"
    liftIO $ do
      BSL.hPutStr rfh . Aeson.encode $ isiRedeemer
      hClose rfh

    datumFlags <- case isiDatum of
      Datum x -> do
        (datumFile, dfh) <- managedSystemTempFile "datum.json"
        liftIO $ do
          BSL.hPutStr dfh $ Aeson.encode x
          hClose dfh

        pure ["--tx-in-datum-file", datumFile]

      InlineDatum -> pure ["--tx-in-inline-datum-present"]

    pure
      $ [ "--tx-in-script-file"
        , isiScript
        ]
      <> datumFlags
      <> [ "--tx-in-redeemer-file"
         , redeemerFile
        ]

toInputFlags :: Input -> Managed [String]
toInputFlags Input {..} =
  mappend ["--tx-in", pprUtxo iUtxo] <$> toScriptFlags iScriptInfo

pprUtxo :: UTxO -> String
pprUtxo UTxO{..} = utxoTx <> "#" <> show utxoIndex

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

datumToOutputs :: OutputDatum -> [String]
datumToOutputs = \case
  NoOutputDatum -> []
  OutputDatumHash dh -> ["--tx-out-datum-hash", dh]
  OutputDatumValue d -> ["--tx-out-datum-embed-value", pprJson d]
  OutputDatumInlineValue d -> ["--tx-out-inline-datum-value", pprJson d]

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
    [ ["transaction", "build", "--babbage-era"]
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

data EvalConfig = EvalConfig
  { ecOutputDir      :: Maybe FilePath
  , ecTestnet        :: Maybe Integer
  , ecProtocolParams :: Maybe FilePath
  } deriving (Show, Eq, Generic)

instance Semigroup EvalConfig where
  x <> y = EvalConfig
    { ecOutputDir      = ecOutputDir      x <|> ecOutputDir      y
    , ecTestnet        = ecTestnet        x <|> ecTestnet        y
    , ecProtocolParams = ecProtocolParams x <|> ecProtocolParams y
    }

instance Monoid EvalConfig where
  mempty = EvalConfig Nothing Nothing Nothing

eval :: EvalConfig -> Tx () -> IO String
eval EvalConfig {..} (Tx m) =
  let
    runCardanoCli args = do
      print args
      (exitCode, outStr) <- readProcessInterleaved . proc "cardano-cli" $ args
      case exitCode of
        ExitSuccess -> pure $ BSLC.unpack outStr
        ExitFailure _ -> liftIO . throwIO . EvalException "cardano-cli" args . BSLC.unpack $ outStr

  in flip with pure $ do
    tempDir <- maybe (managed (withSystemTempDirectory "tx-builder")) pure ecOutputDir
    txBuilder <- liftIO . execStateT (runReaderT m ecTestnet) $ mempty
    bodyFlags <- transactionBuilderToBuildFlags tempDir ecTestnet ecProtocolParams txBuilder

    liftIO $ do
      void $ runCardanoCli bodyFlags
      let
        bodyFile = toSigningBodyFlags tempDir
      -- get the txid
      txId <- fmap init $ runCardanoCli $ ["transaction", "txid"] <> bodyFile

      void . runCardanoCli . transactionBuilderToSignFlags tempDir ecTestnet $ txBuilder

      void . runCardanoCli . mconcat $
        [ [ "transaction", "submit" ]
        , toTestnetFlags ecTestnet
        , ["--tx-file", tempDir </> "signed-body.txt"]
        ]

      pure $ txId
