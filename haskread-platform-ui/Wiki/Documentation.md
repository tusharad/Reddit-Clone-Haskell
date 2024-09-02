### TODO:

- Create Package.dhall to maintain same version of package-sets.
- Create `create thread` page.
- In create thread, community IDs are hard coded.


### Get Current User Flow:

- Read browser localStorage.
- If token is not their, return Nothing.
- If token found. Send mkAuthRequest with bearer token. To get userInfo.
- Send returned data in `Just`.
