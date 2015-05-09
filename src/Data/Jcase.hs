{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Data.Vector                    (Vector)
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit                     as HU

data Jsuite a = Jsuite
  { _jsDescription :: Maybe Text
  , _jsCases       :: Vector a
  } deriving (Eq, Show)

data Jcase a b = Jcase
  { _jcDescription :: Maybe Text
  , _jcContext     :: Maybe a
  , _jcAssertions  :: Vector b
  } deriving (Eq, Show)

data Jassertion a b = Jassertion
  { _jaInput  :: a
  , _jaOutput :: b
  } deriving (Eq, Show)

hUnitJsuite
  :: (FromJSON a, FromJSON b, FromJSON c, Eq a, Eq b, Show a, Show b)
  => (Maybe c -> a -> b)
  -> Jsuite (Jcase c (Jassertion a b))
  -> Test
hUnitJsuite f js = do
  let desc = T.unpack $ fromMaybe "" (_jsDescription js)
  testCase desc (traverse_ g $ _jsCases js)
  where
    -- g :: Jcase c (Jassertion a b) -> HU.Assertion
    g jc = do
      let str = T.unpack $ fromMaybe "" (_jcDescription jc)
      traverse_
        (\ja -> HU.assertEqual str (_jaOutput ja) $ f (_jcContext jc) (_jaInput ja))
        (_jcAssertions jc)

hUnitSimple
  :: (FromJSON a, FromJSON b, Eq a, Eq b, Show a, Show b)
  => (a -> b)
  -> Jsuite (Jcase (Maybe Value) (Jassertion a b))
  -> Test
hUnitSimple f js = hUnitJsuite (const f) js

stdinJsuite
  :: (FromJSON a, FromJSON b, FromJSON c, Eq a, Eq b, Show a, Show b)
  => (Maybe c -> a -> b)
  -> IO ()
stdinJsuite f = do
  b <- B.getContents
  case eitherDecode b of
    Left e                                            -> error e
    Right (js :: (Jsuite (Jcase c (Jassertion a b)))) -> traverse_ g (_jsCases js) >> putStrLn "Success"
  where
    -- g :: Jcase c (Jassertion a b) -> IO ()
    g jc = traverse_ (k jc) $ _jcAssertions jc

    -- k :: Jcase c (Jassertion a b) -> Jassertion a b -> IO ()
    k jc ja = do
      let actual = f (_jcContext jc) (_jaInput ja)
      when (actual /= (_jaOutput ja)) $ error (msg jc ja actual)

    -- msg :: (Show a, Show b) => Jcase c (Jassertion a b) -> Jassertion a b -> a -> String
    msg jc ja v =
      let desc = T.unpack $ fromMaybe "" (_jcDescription jc)
      in unlines $ (if null desc then [] else [desc]) <>
        [ "expected: " <> show (_jaOutput ja)
        , "but got: "  <> show v
        ]

stdinSimple
  :: (FromJSON a, FromJSON b, Eq a, Eq b, Show a, Show b)
  => (a -> b)
  -> IO ()
stdinSimple f = stdinJsuite g
  where
    g c =
      let _ = c :: Maybe Value
      in f

$(deriveFromJSON defaultOptions { fieldLabelModifier = map toLower . drop 3 } ''Jsuite)
$(deriveFromJSON defaultOptions { fieldLabelModifier = map toLower . drop 3 } ''Jcase)
$(deriveFromJSON defaultOptions { fieldLabelModifier = map toLower . drop 3 } ''Jassertion)
