module Platform.Comment.Utils (buildNestedComments) where

import qualified Data.List as L
import Data.Maybe (isNothing)
import Platform.DB.Model

-- Build a nested comment structure
buildNestedComments :: [CommentInfo] -> [NestedComment]
buildNestedComments comments =
  let (roots, others) = L.partition (isNothing . parentCommentIDForCommentInfo) comments
   in map (`buildTree` others) roots

-- Build a tree from a comment and the remaining comments
buildTree :: CommentInfo -> [CommentInfo] -> NestedComment
buildTree comment comments =
  let children0 = filter ((== Just (commentIDForCommentInfo comment)) . parentCommentIDForCommentInfo) comments
   in NestedComment comment (map (`buildTree` comments) children0)

{-
ghci> import Data.Time
ghci> t <- getCurrentTime
ghci> let x = Comment (CommentID 1) (UserID 1) (ThreadID 1) "asd" (Nothing) t t
ghci> let y = Comment (CommentID 2) (UserID 1) (ThreadID 1) "asd" (Just $ CommentID 1) t t
ghci> let z = Comment (CommentID 3) (UserID 1) (ThreadID 1) "asd" (Just $ CommentID 2) t t
ghci> buildNestedComments [x,y,z]
[NestedComment {mainComment = Comment {commentID = 1, userIDForComment = 1, threadIDForComment = 1, commentContent = "asd", parentCommentID = Nothing, createdAtForComment = 2024-08-14 06:53:27.96777373 UTC, updatedAtForComment = 2024-08-14 06:53:27.96777373 UTC}, children = [NestedComment {mainComment = Comment {commentID = 2, userIDForComment = 1, threadIDForComment = 1, commentContent = "asd", parentCommentID = Just 1, createdAtForComment = 2024-08-14 06:53:27.96777373 UTC, updatedAtForComment = 2024-08-14 06:53:27.96777373 UTC}, children = [NestedComment {mainComment = Comment {commentID = 3, userIDForComment = 1, threadIDForComment = 1, commentContent = "asd", parentCommentID = Just 2, createdAtForComment = 2024-08-14 06:53:27.96777373 UTC, updatedAtForComment = 2024-08-14 06:53:27.96777373 UTC}, children = []}]}]}]
ghci>

-}
