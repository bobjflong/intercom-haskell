
### Getting Started

```haskell
> :set -XOverloadedStrings

-- Create a client
> let client = defaultClient & appId .~ "foo" & apiKey .~ "bar"

-- Test your connection
> withIntercom client ping
Status {statusCode = 200, statusMessage = "OK"}
```

### Users

```haskell
> withIntercom client userList
Just (UserList {_users = [User {_name = "bob" ...
```

Pagination:

```haskell
-- Grabs the next page of users from your user list (cycles around at the end)
> withIntercom client $ nextPage userList
Just (UserList {_users = [User {_name = "jim" ...
```

Creating/updating:

```haskell
> let myUser = blankUser & email .~ (Just "bob@foo.com")
> withIntercom client $ createOrUpdateUser myUser
Just (User {_name = Nothing, _email = Just "bob@foo.com", _userId = Nothing, _customAttributes = fromList []})
```
