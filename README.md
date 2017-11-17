# GHC Bome REST

Provides HTTP server with REST API for
[GHC Bome Service](http://github.com/aialferov/ghc-bome).

## API

The REST API provides CRUD operations with user data and exposes the following
endpoint:

```
/v1/users/:id
```

Requests to any other endpoints respond with "400 Bad Request".

### Create

Create user and initialize it with provided data. Or if user exists, overwrite
existing data.

```
# Request:
PUT /v1/users/:id
{
    "<type1>" : <value1>
    ...
    "<typeN>" : <valueN>
}

# Response (if new user was created):
201 Created

# Response (if existing user data was overwritten):
204 No Content

# Response (if body is malformed)
400 Bad Request
{"reason" : "malformed_json"}
```

### Read

Get data of the specified user:

```
# Request
GET /v1/users/:id[?filter=type1,...,typeN]

# Response (if user exists)
200 OK
{
    "<type1>" : <value1>
    ...
    "<typeN>" : <valueN>
}

# Response (if user not found)
404 Not Found

# Response (if query is malformed)
400 Bad Request
{"reason" : "malformed_query"}
```

### Update

Update the specified user data:

```
# Request
PATCH /v1/users/:id
{
    "<type1>" : <value1>
    ...
    "<typeN>" : <valueN>
}

# Response (if user exists)
204 No Content

# Response (if user not found)
404 Not Found

# Response (if body is malformed)
400 Bad Request
{"reason" : "malformed_json"}
```

### Delete

Delete the specified user data:

```
# Request
DELETE /v1/users/:id
{
    "<type1>" : <value1>
    ...
    "<typeN>" : <valueN>
}

# Response (if user exists)
204 No Content

# Response (if user not found)
404 Not Found

# Response (if body is malformed)
400 Bad Request
{"reason" : "malformed_json"}
```

## Run

Although the application should be used within the GHC Bome Service it also
could run on its own, for instance for debug purposes. The following command
runs the application in an Erlang shell:

```
$ make shell
```

Once application is running it provides the API on port 8080. This could be
configured in "app" file or overriden in any other appropriate way (e.g. using
a custom "sys.config" file or by "application:set_env/3,4").

### Backend

Default backend just prints out the requests API receives and does not actually
store any data. The backend could also be configured by specifying the backend
main module in "app" file or overriden by "sys.config" or
"application:set_env/3,4".

### Tests

Run API acceptance tests:

```
$ make at
```

and see results in browser:

```
$ make at-display
```
