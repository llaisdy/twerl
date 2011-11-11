-module(stream_client_util_spec).
-export([spec/0, test/0]).

-include_lib("espec/include/espec.hrl").
-include_lib("eunit/include/eunit.hrl").

test() ->
    espec:run([?MODULE]).

spec() ->
    [
        ?_describe("#generate_headers", [
                ?_it("should generate Host and User-Agent headers", fun() ->
                            Result = stream_client_util:generate_headers(),
                            Expected = [
                                {"Host", "api.twitter.com"},
                                {"User-Agent", "Twerl"}
                            ],

                            ?assertEqual(Expected, Result)
                    end)
            ]),

        ?_describe("#generate_auth_headers", [
                ?_it("should include default headers if none passed", fun() ->
                            Result = stream_client_util:generate_auth_headers("user", "pass"),
                            Headers = stream_client_util:generate_headers(),
                            Expected = [
                                {"Authorization", "Basic " ++ binary_to_list(base64:encode("user" ++ ":" ++ "pass"))} | Headers
                            ],

                            ?assertEqual(Expected, Result)
                    end),

                ?_it("should allow custom headers to be passed", fun() ->
                            Result = stream_client_util:generate_auth_headers("user", "pass", []),
                            Expected = [
                                {"Authorization", "Basic " ++ binary_to_list(base64:encode("user" ++ ":" ++ "pass"))}
                            ],

                            ?assertEqual(Expected, Result)
                    end)
            ]),

        ?_describe("#userids_to_follow", [
                ?_it("should return an error when no users are passed", fun() ->
                            Result = stream_client_util:userids_to_follow([]),
                            Expected = {error, no_args_passed},

                            ?assertEqual(Expected, Result)
                    end),

                ?_it("should return the correct url for one user", fun() ->
                            Result = stream_client_util:userids_to_follow(["1"]),
                            Expected = {ok, "follow=1"},
                            ?assertEqual(Expected, Result)
                    end),

                ?_it("should return the correct url for two users", fun() ->
                            Result = stream_client_util:userids_to_follow(["1", "2"]),
                            Expected = {ok, "follow=1,2"},
                            ?assertEqual(Expected, Result)
                    end)
            ])
    ].
