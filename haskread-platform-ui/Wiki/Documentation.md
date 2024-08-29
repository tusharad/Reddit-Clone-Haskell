### TODO:

- Create Package.dhall to maintain same version of package-sets.


### Get Current User Flow:

- Read browser localStorage.
- If token is not their, return Nothing.
- If token found. Send mkAuthRequest with bearer token. To get userInfo.
- Send returned data in `Just`.
