## HaskRead-Platform-be

## User

Features list:

- User Registration
- User Login
- User Profile
- Change password
- Delete account
- Update user info

- ### User Registration
  ###### POST /api/v1/user/auth/register
         JSON Body:
         {
            "userName": "string",
            "email": "string",
            "password": "string",
            "confirmPassword": "string"
         }
  - User can register by providing userName, email, password and confirmPassword.
  - On successful registration, the API shall return a success message including the user details.
  - Checks:
    - Email should be unique (Email in the database shall not exist).
    - Password and confirmPassword should match.
    - Password should be at least 8 characters long.
    - Email should be valid.
    - userName should be unique (userName in the database shall not exist).
  - On any of the above checks fail, the API shall return an error message with status code 400.

- Future Features
    - Verify email using send mail API.

- ### User Login
    ###### POST /api/v1/user/auth/login
           JSON Body:
           {
              "email": "string",
              "password": "string"
           }
    - User can login by providing email and password.
    - On successful login, the jwt token shall be returned as a response. The jwt token shall also be attached in the header for further requests. The jwt token shall be valid for 4 hours.
    - Checks:
        - Email should exist in the database.
        - Password should match the password in the database.
        - Make sure, incoming request is not loggedIn.
    - On any of the above checks fail, the API shall return an error message with status code 400.

- ## User protected routes
    - All the routes mentioned below are protected and require jwt token in the header.
    - If the jwt token is not provided in the header, the API shall return an error message with status code 401.
    - If the jwt token is invalid or expired, the API shall return an error message with status code 401.
    - If the jwt token is valid, the API shall return the response as expected.
    - Some of the APIs require any user to be logged in, such as create post, upvote/downvote etc. While some APIs require the user to be the owner of the resource, such as delete post, update post etc.
    - The APIs that require the user to be the owner of the resource, shall check the user id in the jwt token and the user id in the resource. If both are the same, the API shall proceed with the request, otherwise, the API shall return an error message with status code 401.

- ### User Profile
    ###### GET /api/v1/user/profile
    - User can view his/her profile by providing the jwt token in the header.
    - On successful request, the API shall return the user details.
    - Checks:
        - User should be logged in.
    - On failure, the API shall return an error message with status code 400.

- ### Change password
    ###### PUT /api/v1/user/change-password
    JSON Body:
    {
        "currentPassword": "string",
        "newPassword": "string",
        "confirmPassword": "string"
    }
    - User can change his/her password by providing the current password, new password and confirmPassword.
    - On successful request, the API shall return a success message.
    - Checks:
        - User should be logged in.
        - Current password should match the password in the database.
        - New password and confirmPassword should match.
        - New password should be at least 8 characters long.
        - New password and current password should not be the same.
    - On failure, the API shall return an error message with status code 400.

- ### Delete account
    ###### DELETE /api/v1/user/delete-account
    - User can delete his/her account by providing the password.
    - On successful request, the API shall return a success message.
    - Checks:
        - User should be logged in.
        - Password should match the password in the database.
    - On failure, the API shall return an error message with status code 400.
    - At application level, the user record shall be delete from the primary table and all the related records shall be deleted from the related tables. And the user shall be logged out. The user info shall be added in some kind of archive table for future reference.

- Future Features
    - Add, update, delete profile image.

- ### Database Schema
    - User Table
        - id : primary key
        - userName : unique
        - email : unique
        - password : hashed
        - createdAt : timestamptz
        - updatedAt : timestamptz

    - User Profile Image Table
        - userID : foreign key to user table on delete cascade
        - image : blob
        - createdAt : timestamptz
        - updatedAt : timestamptz

    - Constraints
        - (email,password) should be indexed.
        - trigger on user and profile to update updatedAt on insert and update.

## Admin

Features list:
    - Admin Login
    - Admin Profile
    - Change password
    - Create admin

- ### Admin Login
    ###### POST /api/v1/admin/auth/login
    - Admin can login by providing email and password.
    - On successful login, the jwt token shall be returned as a response. The jwt token shall also be attached in the header for further requests. The jwt token shall be valid for 4 hours.
    - Checks:
        - Email should exist in the database.
        - Password should match the password in the database.
        - Make sure, incoming request is not loggedIn.
    - On any of the above checks fail, the API shall return an error message with status code 400.

- ### Admin Profile
    ###### GET /api/v1/admin/profile
    - Admin can view his/her profile by providing the jwt token in the header.
    - On successful request, the API shall return the admin details.
    - Checks:
        - Admin should be logged in.
    - On failure, the API shall return an error message with status code 400.

- ### Change password
    ###### PUT /api/v1/admin/change-password
    - Admin can change his/her password by providing the current password, new password and confirmPassword.
    - On successful request, the API shall return a success message.
    - Checks:
        - Admin should be logged in.
        - Current password should match the password in the database.
        - New password and confirmPassword should match.
        - New password should be at least 8 characters long.
        - New password and current password should not be the same.
    - On failure, the API shall return an error message with status code 400.

- ### Create admin
    ###### POST /api/v1/admin/create-admin
    - Admin can create another admin by providing email, password and confirmPassword.
    - On successful request, the API shall return a success message.
    - Checks:
        - Admin should be logged in.
        - Email should be unique (Email in the database shall not exist).
        - Password and confirmPassword should match.
        - Password should be at least 8 characters long.
        - Email should be valid.
    - On any of the above checks fail, the API shall return an error message with status code 400.

- ### Database Schema
    - Admin Table
        - id : primary key
        - email : unique
        - password : hashed
        - createdAt : timestamptz
        - updatedAt : timestamptz

    - Constraints
        - (email,password) should be indexed.
        - trigger on admin to update updatedAt on insert and update.

## Community

Features list:
    - create community
    - update community
    - delete community

- ### Create Community
    ###### POST /api/v1/community/create
    JSON Body:
    {
        "communityName" : "String",
        "communityDescription" : "String",
    }
    - Future Features:
        - Array of tags can be associated with community.
    - Admin can add a certain community.
    - Checks:
        - Community with same name shall not exists in DB.
        - Description and name shall not be empty.
    - On failure on above checks, the API shall return an error message with status code 400.

- ### Update Community
    ###### PUT /api/v1/community/update
    JSON Body:
    {
        "communityID" : Int
      , "communityName" : "String"
      , "communityDescription" : "String"
    }
    - Admin can update any community.
    - Checks:
        - Community with same name shall not exists in DB.
        - Description and name shall not be empty.
    - On failure on above checks, the API shall return an error message with status code 400.

- ### Delete Community
    ###### DELETE /api/v1/community/delete/:communityID
    - Admin can delete any community.
    - Checks:
        - CommunityID shall exists in DB.
    - On failure on above checks, the API shall return an error message with status code 400.

- ### Database Schema
    - Community Table
        - id : primary key
        - communityName : unique
        - communityDescription : text
        - createdAt : timestamptz
        - updatedAt : timestamptz

    - Constraints
        - (communityName) should be indexed.
        - trigger on community to update updatedAt on insert and update.

## Thread -- synonym of post

Features list:
    - Create thread
    - Update thread
    - Delete thread
    - Get thread
    - Get all thread by userid (paginated)
    - Get all thread by community (paginated)
    - Get all thread order by upvote (paginated)
    - Get all thread order by recent (paginated)
    - Upvote/Downvote thread
        - Upvote thread
        - Downvote thread

- ### Create thread
    ###### POST /api/v1/thread/create
    JSON Body:
    {
        "title": "string",
        "content": "string",
        "communityId": "string"
    }
    - Future Features
        - Add, update, delete image,link,video in the thread.
    - User can create a thread by providing title, content and communityId.
    - On successful request, the API shall return the thread details.
    - Checks:
        - User should be logged in.
        - CommunityId should exist in the database.
        - Title should not be empty.
        - Title can be at most 100 characters long (frontend should also have this check).
    - On failure, the API shall return an error message with status code 400.

- ### Update thread
    ###### PUT /api/v1/thread/update/:threadId
    - User can update his/her thread by providing threadId, title and content.
    - On successful request, the API shall return the thread details.
    - Checks:
        - ThreadID should exist in the database.
        - User should be logged in.
        - User should be the owner of the thread.
        - Title should not be empty.
        - Title can be at most 100 characters long (frontend should also have this check).
    - On failure, the API shall return an error message with status code 400.

- ### Delete thread
    ###### DELETE /api/v1/thread/delete/:threadId
    - User can delete his/her thread by providing threadId.
    - On successful request, the API shall return a success message.
    - Checks:
        - User should be logged in.
        - ThreadID should exist in the database.
        - User should be the owner of the thread.
    - On failure, the API shall return an error message with status code 400.

- ### Get thread
    ###### GET /api/v1/thread/get/:threadId
    - User can view a thread by providing threadId.
    - On successful request, the API shall return the thread details.
    - Checks:
        - ThreadID should exist in the database.
    - On failure, the API shall return an error message with status code 400.

- ### Get all thread by userid (paginated)
    ###### GET /api/v1/thread/get-all-by-userid/:userId?page=1&limit=10
    - User can view all his/her threads by providing userId, page and limit.
    - On successful request, the API shall return the thread details.
    - Checks:
        - UserId should exist in the database.
        - page and limit should be valid. I.e page should be greater than 0 and limit should be greater than 0.
        - If the page has no data, the API shall return an empty array.
    - On failure, the API shall return an error message with status code 400.

- ### Get all thread by community (paginated)
    ###### GET /api/v1/thread/get-all-by-community/:communityId?page=1&limit=10
    - User can view all threads by providing communityId, page and limit.
    - On successful request, the API shall return the thread details.
    - Checks:
        - CommunityId should exist in the database.
        - page and limit should be valid. I.e page should be greater than 0 and limit should be greater than 0.
        - If the page has no data, the API shall return an empty array.
    - On failure, the API shall return an error message with status code 400.

- ### Get all thread order by upvote (paginated)
    ###### GET /api/v1/thread/get-all-by-upvote?page=1&limit=10
    - User can view all threads order by upvote, by providing page and limit.
    - On successful request, the API shall return the thread details.
    - Checks:
        - page and limit should be valid. I.e page should be greater than 0 and limit should be greater than 0.
        - If the page has no data, the API shall return an empty array.
    - On failure, the API shall return an error message with status code 400.

- ### Get all thread order by recent (paginated)
    ###### GET /api/v1/thread/get-all-by-recent?page=1&limit=10
    - User can view all threads order by recent, by providing page and limit.
    - On successful request, the API shall return the thread details.
    - Checks:
        - page and limit should be valid. I.e page should be greater than 0 and limit should be greater than 0.
        - If the page has no data, the API shall return an empty array.
    - On failure, the API shall return an error message with status code 400.