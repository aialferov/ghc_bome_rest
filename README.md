# GHC Bome REST

Erlang application implementing HTTP server with REST API for
[GHC Bome Service](http://github.com/aialferov/ghc-bome).

## API

The REST API provides CRUD operations with user metrics and exposes the
following endpoint:

```
/v1/users/:id
```

Requests to any other endpoints respond with "400 Bad Request".

### Create

Create user and initialize it with provided metrics. Overwrites existing metrics
if user exists.

```
# Request:
PUT /v1/users/:id
{
    "<type1>" : <metric_value1>,
    ...,
    "<typeN>" : <metric_valueN>
}

# Response (if new user was created):
201 Created

# Response (if existing user metrics were overwritten):
204 No Content

# Response (if body is malformed)
400 Bad Request
{"reason" : "malformed_json"}
```

### Read

Get metrics of the specified user:

```
# Request
GET /v1/users/:id[?filter=metric_name1,...,metric_nameN]

# Response (if user exists)
200 OK
{
    "<metric_name1>" : <metric_value1>,
    ...,
    "<metric_nameN>" : <metric_valueN>
}

# Response (if user not found)
404 Not Found

# Response (if query contains unknown option)
400 Bad Request
{"reason" : {"unknown_option" : "<Option>"}}
```

### Update

Update the specified user metrics:

```
# Request
PATCH /v1/users/:id
{
    "<metric_name1>" : <metric_value1>,
    ...,
    "<metric_nameN>" : <metric_valueN>
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
[
    "<metric_name1>",
    ...,
    "<metric_nameN>"
]

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
