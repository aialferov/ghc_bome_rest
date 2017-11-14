# GHC Bome REST

Provides HTTP server with REST API for GHC Bome Service.

## API

The REST API provides interface to put, get and delete user data.

Put data:
```
PUT /v1/<user>
{"<type>": "<value>"}
```

Get data:
```
GET /v1/<user>
GET /v1/<user>/<type>
```

Delete data:
```
DELETE /v1/<user>
DELETE /v1/<user>/<type>
```

### Debug

To debug or play with the API functions run the application in an Erlang shell:
```
$ make shell
```

Once application is running it provides API on port 8080. You can use any HTTP
client to generate queries mentioned above.
