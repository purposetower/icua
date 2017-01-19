{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Core.JSONExample where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BSL

data SomeData = SomeData {
    someData :: [Text]
} deriving (Generic, Show)

instance FromJSON SomeData
instance ToJSON SomeData

main :: IO ()
main = do
    putStrLn $ BSL.unpack $ encode (SomeData ["hello", "bye"])

    let decoded = decode (BSL.pack "{\"someData\":[\"dd\",\"ff\"]}") :: Maybe SomeData

    case decoded of
        Just x -> putStrLn $ show x
        Nothing -> return ()

