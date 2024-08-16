module Data.Thread
  where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec ((>~>))
import Data.Codec.Argonaut.Migration as CAM

import Data.Codec.Argonaut.Record as CAR

type ThreadRep row = {
    threadTitle :: String,
    threadContent :: String
    | row
 }

type Thread = {
   threadTitle :: String,
   threadContent :: String
 }

threadCodec :: JsonCodec Thread
threadCodec =
   CAR.object "thread" 
   {  threadTitle : CA.string,
      threadContent : CA.string
   }

type PaginatedArray a =
  { total :: Int
  , body :: Array a
  }

threadsCodec :: JsonCodec (PaginatedArray Thread)
threadsCodec =  
   CAM.renameField "articles" "body"
    >~> CAM.renameField "articlesCount" "total"
    >~> codec
  where
  codec =
    CAR.object "PaginatedArray ArticleWithMetadata"
      { body: CA.array (threadCodec)
      , total: CA.int
      }