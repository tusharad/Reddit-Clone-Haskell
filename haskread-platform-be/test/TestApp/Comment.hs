{-# LANGUAGE OverloadedStrings #-}

module TestApp.Comment (commentAPITests, voteCommentAPITests) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types
import Servant
import Test.Tasty
import Test.Tasty.Wai
import TestApp.SampleData

voteCommentAPITests :: Application -> [BSL.ByteString] -> TestTree
voteCommentAPITests app userToken =
  testGroup
    "vote comment APIs"
    [testVoteCommentAPI app (Prelude.head userToken)]

testVoteCommentAPI :: Application -> BSL.ByteString -> TestTree
testVoteCommentAPI app token =
  testWai app "/vote comment - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            PUT
            "/api/v1/user/comment/vote/3/true"
            ""
            [ ("Content-Type", "application/json"),
              (hAuthorization, "Bearer " <> BSL.toStrict token)
            ]
        )
    assertStatus' status200 res

commentAPITests :: Application -> [BSL.ByteString] -> TestTree
commentAPITests app userToken =
  testGroup
    "comment APIs"
    [ testCreateCommentAPI app (Prelude.head userToken),
      testUpdateCommentAPI app (Prelude.head userToken),
      testDeleteCommentAPI app (Prelude.head userToken)
    ]

testCreateCommentAPI :: Application -> BSL.ByteString -> TestTree
testCreateCommentAPI app token =
  testWai app "/create comment - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            POST
            "/api/v1/user/comment/create"
            commentCreateReqBody
            [ ("Content-Type", "application/json"),
              (hAuthorization, "Bearer " <> BSL.toStrict token)
            ]
        )
    assertStatus' status200 res

testUpdateCommentAPI :: Application -> BSL.ByteString -> TestTree
testUpdateCommentAPI app token =
  testWai app "/update comment - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            PUT
            "/api/v1/user/comment/update/3"
            commentUpdateReqBody
            [ ("Content-Type", "application/json"),
              (hAuthorization, "Bearer " <> BSL.toStrict token)
            ]
        )
    assertStatus' status200 res

testDeleteCommentAPI :: Application -> BSL.ByteString -> TestTree
testDeleteCommentAPI app token =
  testWai app "/delete comment - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            DELETE
            "/api/v1/user/comment/delete/2"
            ""
            [ ("Content-Type", "application/json"),
              (hAuthorization, "Bearer " <> BSL.toStrict token)
            ]
        )
    assertStatus' status200 res
