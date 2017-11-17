-record(state, {db_module}).

-define(ContentTypeText, #{<<"content-type">> => <<"text/plain">>}).
-define(ContentTypeJson, #{<<"content-type">> => <<"application/json">>}).

-define(CodeOk, 200).
-define(CodeCreated, 201).
-define(CodeNoContent, 204).

-define(CodeBadRequest, 400).
-define(CodeNotFound, 404).
