-module(quicktest).
-export([test/1, test/2]).

test(UP) ->
    test(UP, ["erlang"]).

test({Username, Password}, KeyWordList) ->
    Headers = stream_client_util:generate_auth_headers(Username, Password),
    {ok, Params} = stream_client_util:keywords_to_track(KeyWordList),
    Callback = fun(Data) ->
		       Tweet = proplists:get_value(<<"text">>, Data),
		       io:format("Erlang <3: ~s~n", [Tweet])
	       end,
    stream_client:connect(stream_client_util:filter_url(), 
			  Headers, Params, Callback).

