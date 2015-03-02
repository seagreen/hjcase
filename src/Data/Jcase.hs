{-# LANGUAGE TemplateHaskell #-}

module Data.Jcase where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy           as B
import           Data.Char                      (toLower)
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Data.Vector                    (Vector)
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit                     as HU

data Jcase = Jcase
  { _jcDescription :: Maybe Text
  , _jcData        :: Maybe Value
  , _jcAssertions  :: Vector JcaseAssertion
  } deriving (Eq, Show)

data JcaseAssertion = JcaseAssertion
  { _description :: Maybe Text
  , _input       :: Value
  , _output      :: Value
  } deriving (Eq, Show)

jcaseToHUnit :: (Maybe Value -> Value -> Value) -> Jcase -> Test
jcaseToHUnit f jc = do
  let desc = T.unpack $ fromMaybe "" (_jcDescription jc)
  testCase desc (traverse_ g $ _jcAssertions jc)
  where
    g :: JcaseAssertion -> HU.Assertion
    g ja = do
      let str = T.unpack $ fromMaybe "" (_description ja)
      HU.assertEqual str
        (_output ja) $
        f (_jcData jc) (_input ja)

stdinJcase :: (Maybe Value -> Value -> Value) -> IO ()
stdinJcase f = do
  b <- B.getContents
  case eitherDecode b of
    Left e   -> error e
    Right jc -> do
      traverse_ (g $ _jcData jc) (_jcAssertions jc)
      TIO.putStrLn "Success"
  where
    g :: Maybe Value -> JcaseAssertion -> IO ()
    g maybeData ja = do
      let actual = f maybeData (_input ja)
      unless (actual == _output ja) $ error (msg ja actual)

    msg :: JcaseAssertion -> Value -> String
    msg ja v =
      let desc = T.unpack $ fromMaybe "" (_description ja)
      in unlines $ (if null desc then [] else [desc]) <>
        [ "expected: " <> show (_output ja)
        , "but got: "  <> show v
        ]

$(deriveFromJSON defaultOptions { fieldLabelModifier = map toLower . drop 3 } ''Jcase)
$(deriveFromJSON defaultOptions { fieldLabelModifier = drop 1 } ''JcaseAssertion)
