-define(Usage, <<
    "Usage:\n"
    "\n"
    "PUT /v1/<user>\n"
    "{\"type\": \"value\"}\n"
    "\n"
    "GET /v1/<user>\n"
    "GET /v1/<user>/<type>\n"
    "\n"
    "DELETE /v1/<user>\n"
    "DELETE /v1/<user>/<type>\n"
>>).

-define(ContentTypeText, #{<<"content-type">> => <<"text/plain">>}).
-define(ContentTypeJson, #{<<"content-type">> => <<"application/json">>}).

-define(CodeOk, 200).
-define(CodeCreated, 201).
-define(CodeNoContent, 204).

-define(CodeBadRequest, 400).
