import           Test.Hspec
import           Data.String.Here.Uninterpolated
import           Data.String.Here.Interpolated
import qualified Cardano.Api.Shelley as S
import           Cardano.Transaction
import           Text.Megaparsec
import qualified Data.Map.Strict as M

main :: IO ()
main = hspec $ describe "UTxO parser tests" $ do
  let
    inputScriptData = [here|
        ( ScriptDataConstructor 0
            [ ScriptDataBytes "2\222A$a^\183\&9\213\156\\s=\FS\207\RS\ENQ&\234\135\agp\224\183\186F\154"
            , ScriptDataNumber 1662058473000
            , ScriptDataNumber 1662058593000
            , ScriptDataNumber 8000000
            , ScriptDataMap
                [ ( ScriptDataBytes "2\222A$a^\183\&9\213\156\\s=\FS\207\RS\ENQ&\234\135\agp\224\183\186F\154"
                  , ScriptDataNumber 900
                  )
                , ( ScriptDataBytes "\231\147\237U\231\141\162\220\216\197\249V2\253\211\CAN\DELd\195$Fh\253\218G%9\n"
                  , ScriptDataNumber 50
                  )
                , ( ScriptDataBytes "\STX\234\230u\202rc\DLE\\\239J\253\230\133\194\ESC\190\128\242>\167t\181\160\184\131H#"
                  , ScriptDataNumber 50
                  )
                ]
            , ScriptDataConstructor 1 []
            , ScriptDataMap [
              ( ScriptDataBytes "\214\207\219\237\210B\ENQft\192\229\RS\173\SOHxT\151\227\164\138\251\187\DC4m\199.\225\226"
              , ScriptDataMap [(ScriptDataBytes "\DC24V",ScriptDataNumber 1)]
              )
            ]
            , ScriptDataBytes "G<\238c\198\\\150\155X/\186\183\177#\167\179\190\233x\SOHD\SYNo\251\255\193)\165"
            , ScriptDataBytes "\244\&5\245\203\208\NUL\ENQ\234}W\229\182\160W&5;\200\244\213\248A\176\187\128\&5\230y"
          ]
        )
      |]

    expectedScriptData = S.scriptDataToJson S.ScriptDataJsonDetailedSchema $ S.ScriptDataConstructor 0
          [ S.ScriptDataBytes "2\222A$a^\183\&9\213\156\\s=\FS\207\RS\ENQ&\234\135\agp\224\183\186F\154"
          , S.ScriptDataNumber 1662058473000
          , S.ScriptDataNumber 1662058593000
          , S.ScriptDataNumber 8000000
          , S.ScriptDataMap
              [ ( S.ScriptDataBytes "2\222A$a^\183\&9\213\156\\s=\FS\207\RS\ENQ&\234\135\agp\224\183\186F\154"
                , S.ScriptDataNumber 900
                )
              , ( S.ScriptDataBytes "\231\147\237U\231\141\162\220\216\197\249V2\253\211\CAN\DELd\195$Fh\253\218G%9\n"
                , S.ScriptDataNumber 50
                )
              , ( S.ScriptDataBytes "\STX\234\230u\202rc\DLE\\\239J\253\230\133\194\ESC\190\128\242>\167t\181\160\184\131H#"
                , S.ScriptDataNumber 50
                )
              ]
          , S.ScriptDataConstructor 1 []
          , S.ScriptDataMap [
            ( S.ScriptDataBytes "\214\207\219\237\210B\ENQft\192\229\RS\173\SOHxT\151\227\164\138\251\187\DC4m\199.\225\226"
            , S.ScriptDataMap [(S.ScriptDataBytes "\DC24V",S.ScriptDataNumber 1)]
            )
          ]
          , S.ScriptDataBytes "G<\238c\198\\\150\155X/\186\183\177#\167\179\190\233x\SOHD\SYNo\251\255\193)\165"
          , S.ScriptDataBytes "\244\&5\245\203\208\NUL\ENQ\234}W\229\182\160W&5;\200\244\213\248A\176\187\128\&5\230y"
        ]

  it "parses a inline datum" $
    Just expectedScriptData `shouldBe` scriptDataStringToJson inputScriptData

  it "parses the tx id" $ do
    let
      initial = [i|7b8094583b602d196df51125a5cf74aef0a4603d3b8d8690e25a007262a00113|]

      expected = "7b8094583b602d196df51125a5cf74aef0a4603d3b8d8690e25a007262a00113"

    parseMaybe parseTxId initial `shouldBe` Just expected

  it "parses the utxo index" $ do
    let
      initial = [i|1|]

      expected = 1

    parseMaybe parseUTxOIndex initial `shouldBe` Just expected

  it "parses the value" $ do
    let
      initial = [i|2000000 lovelace + 1 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456|]

      expected
        = Value
        $ M.insert "" (M.singleton "" 2000000)
        $ M.singleton "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
        $ M.singleton "123456" 1

    parseValue initial `shouldBe` Just expected

  it "parses the datum hash" $ do
    let
      initial = [i|+ TxOutDatumHash ScriptDataInBabbageEra "a302da6ba7bf5a7e159a3f96bf11a345afd5dc1b6a87145a273180428f6cf703"|]

      expected = UTxO_DatumHash "a302da6ba7bf5a7e159a3f96bf11a345afd5dc1b6a87145a273180428f6cf703"

    parseEmtpyState initial parseDatum `shouldBe` Right expected

  it "parses a full line: datum hash" $ do
    let
      initial = [i|7b8094583b602d196df51125a5cf74aef0a4603d3b8d8690e25a007262a00113     1        2000000 lovelace + 1 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456 + TxOutDatumHash ScriptDataInBabbageEra "a302da6ba7bf5a7e159a3f96bf11a345afd5dc1b6a87145a273180428f6cf703"|]

      expected = UTxO
        { utxoIndex  = 1
        , utxoTx     = "7b8094583b602d196df51125a5cf74aef0a4603d3b8d8690e25a007262a00113"
        , utxoValue  =  Value
                      $ M.insert "" (M.singleton "" 2000000)
                      $ M.singleton "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
                      $ M.singleton "123456" 1
        , utxoDatum  = UTxO_DatumHash "a302da6ba7bf5a7e159a3f96bf11a345afd5dc1b6a87145a273180428f6cf703"
        }

    parseUTxOLine initial `shouldBe` Right expected
