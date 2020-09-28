{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Rib.CliSpec
  ( spec,
  )
where

import Options.Applicative
import Relude
import Rib.Cli
import Test.Hspec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "Host and Port parsing" $ do
    let parseHostPort =
          parse hostPortParser "<test>"
    it "should parse port" $ do
      parseHostPort ":8080" `shouldBe` Right ("127.0.0.1", 8080)
    it "should parse localhost" $ do
      parseHostPort "localhost:8080" `shouldBe` Right ("localhost", 8080)
    it "should parse IP addr" $ do
      parseHostPort "132.45.0.254:8080" `shouldBe` Right ("132.45.0.254", 8080)

  describe "parsing directories" $ do
    let inDef = "in_def"
    let outDef = "out_def"
    let cliParserInfo = info (cliParser inDef outDef) mempty

    let parseCli :: [String] -> IO (ParserResult CliConfig)
        parseCli = pure . execParserPure defaultPrefs cliParserInfo

    it "should parse without any arguments" $ do
      Success cli <- parseCli []
      inputDirs cli `shouldBe` one "in_def/"
      outputDir cli `shouldBe` "out_def/"
      shakeDbDir cli `shouldBe` "in_def/.shake"

    it "should allow a shake dir override" $ do
      Success cli <- parseCli ["--shake-dir", "our_shake"]
      inputDirs cli `shouldBe` one "in_def/"
      outputDir cli `shouldBe` "out_def/"
      shakeDbDir cli `shouldBe` "our_shake/.shake"

    it "should parse a single input directory" $ do
      Success cli <- parseCli ["--input-dir", "our_in"]
      inputDirs cli `shouldBe` one "our_in/"
      outputDir cli `shouldBe` "out_def/"
      shakeDbDir cli `shouldBe` "our_in/.shake"

    it "should allow a shake an input dir + shake dir override" $ do
      Success cli <- parseCli ["--input-dir", "our_in", "--shake-dir", "our_shake"]
      inputDirs cli `shouldBe` one "our_in/"
      outputDir cli `shouldBe` "out_def/"
      shakeDbDir cli `shouldBe` "our_shake/.shake"

    it "should allow multiple input directories" $ do
      Success cli <- parseCli ["--input-dir", "in1", "--input-dir", "in2", "--input-dir", "in3"]
      inputDirs cli `shouldBe` fromList ["in1/", "in2/", "in3/"]
      outputDir cli `shouldBe` "out_def/"
      shakeDbDir cli `shouldBe` "in1/.shake"

    it "should allow multiple input directories + shake dir override" $ do
      Success cli <- parseCli ["--input-dir", "in1", "--shake-dir", "our_shake", "--input-dir", "in2"]
      inputDirs cli `shouldBe` fromList ["in1/", "in2/"]
      outputDir cli `shouldBe` "out_def/"
      shakeDbDir cli `shouldBe` "our_shake/.shake"
