
### Getting Started

```haskell
> :set -XOverloadedStrings

-- Create a client
> let client = defaultClient & appId .~ "foo" & apiKey .~ "bar"

-- Test your connection
> ping client
Status {statusCode = 200, statusMessage = "OK"}
```

### Users

```haskell
> userList client
Just (UserList {_users = [User {_name = "bob" ...
```

Pagination:

```haskell
-- Grabs the next page of users from your user list (cycles around at the end)
> nextPage userList client
Just (UserList {_users = [User {_name = "jim" ...
```

Creating/updating:

```haskell
> let myUser = blankUser & email .~ (Just "bob@foo.com")
> createOrUpdateUser myUser client
Just (User {_name = Nothing, _email = Just "bob@foo.com", _userId = Nothing, _customAttributes = fromList []})
```
