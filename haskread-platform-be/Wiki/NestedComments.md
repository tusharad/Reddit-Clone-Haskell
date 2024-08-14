### Implementing Nested/Staircase comment model.

In the discussion board, A comment can have a reply comment which in-itself is a comment and that replyed comment can
have it's own reply as well. The task is how to categorized and display these comments in an efficient manner.

Brefore we discuss how to display this nested comment model, we need to understand how these comments are stored at
the database level.

the simplified comments table looks like this:

```
comment_id : Int
comment_content: Text
parent_comment_id : Int (nullable)
```

Every replyed comment contains a reference to it's parent comment. But the parent comment has no idea how many
children it has.

At application level, the structure would look like this:

```
data Comment = Comment {
    commentID :: Int,
    commentContent :: Text,
    parentCommentID :: Maybe Int
}
```

Assuming we get a list of comments ([Comment]) from the DB. We can create a new type from Comment type, where
each comment will contain a list of it's child comments with itself and those child comments will also contain a
list of child comments in a recursive manner. (Note: Will assume, we will get correct data from the data 
i.e no cycle will appear in comment_id.) If the comment has no replies, it's associated list will simply be empty.

The type for processed comment can look something like this:

```
data NestedComment = NestedComment Comment [NestedComment]
```
