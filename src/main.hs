-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import           Data.Aeson
   ( FromJSON
   , Result (Success, Error)
   , Value (Object)
   , eitherDecode
   , fromJSON
   )
import qualified Data.Aeson as Aeson
import           Data.Aeson.Bson ( toAeson )
import           Data.Bson ( (=:) )
import qualified Data.Bson as Bson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           GHC.Generics ( Generic )


data Creature = Creature
   { _id :: T.Text
   , name :: T.Text
   , breed :: T.Text
   }
   deriving (Generic, Show)

instance FromJSON Creature


jsSmokey :: BL.ByteString
jsSmokey = "{ \"_id\": \"02\", \"name\": \"Smokey\", \"breed\": \"Siamese\" }"


bsCats :: [Bson.Document]
bsCats =
   [  [ "_id" =: ("01" :: T.Text)
      , "name" =: ("Murka" :: T.Text)
      , "breed" =: ("tiger mix" :: T.Text)
      ]
   ,  [ "_id" =: ("02" :: T.Text)
      , "name" =: ("Smokey" :: T.Text)
      , "breed" =: ("Siamese" :: T.Text)
      ]
   -- Not a common case, but to illustrate a bad Document (not an instance of our ADT) in the input
   ,  [ "_id" =: ("03" :: T.Text)
      , "foo" =: ("this record is nonsense" :: T.Text)
      ]
   ,  [ "_id" =: ("04" :: T.Text)
      , "name" =: ("Glamour Kitty" :: T.Text)
      , "breed" =: ("Himalayan" :: T.Text)
      ]
   ]


bsMurka :: Bson.Document
bsMurka = bsCats !! 0


main :: IO ()
main = do
   putStrLn "\nFirst, simple decoding of JSON in a ByteString"
   putStrLn $ "input:\n" ++ (show jsSmokey)
   let js = (eitherDecode jsSmokey) :: Either String Creature
   putStrLn $ "result of Data.Aeson.eitherDecode:\n" ++ (show js)


   putStrLn "\nConverting a Data.Bson.Document into a Data.Aeson.Types.Object with aeson-bson"
   putStrLn $ "input:\n" ++ (show bsMurka)
   let jsMurka = toAeson bsMurka
   putStrLn $ "result of Data.Aeson.Bson.toAeson:\n" ++ (show jsMurka)


   -- Go from a BSON data structure to an ADT

   -- Walking it through a ByteString (by doing encode-decode)
   -- works, but is a terrible method.
   --let crMurka = (eitherDecode . encode . toAeson $ bsMurka) :: Either String Creature
   --print crMurka

   putStrLn "\nGoing from Data.Bson.Document, through Data.Aeson.Value to an ADT"
   putStrLn $ "input:\n" ++ (show bsMurka)
   -- Object is one of Data.Aeson.Value's constructors, this was the missing piece
   putStrLn "The process goes like this (where a is an instance of FromJSON):"
   putStrLn "  Data.Aeson.Result a <- Data.Aeson.Value <- Data.Aeson.Object <- Data.Bson.Document"
   putStrLn "                   fromJSON      .      Object       .       toAeson"
   let rcrMurka = (fromJSON . Object . toAeson $ bsMurka) :: Result Creature
   putStrLn $ "result of (fromJSON . Object . toAeson):\n" ++ (show rcrMurka)


   putStrLn "\nNow a list of them, using convenience functions fromBSON and catResults"
   putStrLn $ "input:\n" ++ (show bsCats)
   let creatures = (catResults . map fromBSON $ bsCats) :: [Creature]
   putStrLn "converted:"
   mapM_ print creatures


{- Convenience function for going directly from a BSON Document to
   an instance of FromJSON
-}
fromBSON :: (FromJSON a) => Bson.Document -> Aeson.Result a
fromBSON = fromJSON . Object . toAeson


{- Convenience function for discarding the Aeson.Result wrapping
   on a list of conversion results
-}
catResults :: [Result a] -> [a]
catResults ((Success x) : rs) = x : catResults rs
catResults ((Error   _) : rs) =     catResults rs
catResults []                 =     []
