-module(api_client).

-export([request/4, get/2, post/2]).

request(Method, Headers, Path, Params) ->
  case host() of
    false -> {error, http_unavailable};
    {Host, Options} ->
      case ulitos_app:get_var(erl_api_client, Path) of
        undefined -> {error, path_not_set};
        RecPath ->
          {NewRecPath, NewParams} = compile_path(RecPath, Params),
          {Url, BodyToSend} =
            case Method of
              get ->
                {hackney_url:make_url(Host, NewRecPath, NewParams), <<"">>};
              _ ->
                {hackney_url:make_url(Host, NewRecPath, []), hackney_url:qs(NewParams)}
            end,    
          case hackney:request(Method, Url, Headers, BodyToSend, Options) of
            {ok, 200, _, ClientRef} ->
              case hackney:body(ClientRef) of
                {ok, Body} ->
                  {JsonResponse} = jiffy:decode(Body),
                  {ok, JsonResponse};
                {error, _Reason} ->
                  {error, hackney_error}
              end;
            {ok, _Other, _, _ClientRef} ->
              {error, http_error};
            {error, _Reason} ->
              {error, req_failed}
          end
      end
  end.

get(Path, Params) ->
    request(get, auth_headers(), Path, Params).

post(Path, Params) ->
    request(post, post_headers(), Path, Params).

host() ->
  {Prefix, Options} = case ulitos_app:get_var(erl_api_client, http_ssl) of
                        true -> {<<"https://">>, []};
                        false -> {<<"http://">>, []};
                        _ -> false
                      end,
  case {ulitos_app:get_var(erl_api_client, http_host), Prefix} of
    {undefined, _} -> false;
    {_, false} -> false;
    {Host, Prefix} ->
      {<<Prefix/binary, Host/binary>>, Options}
  end.

  %%%%%%%%% private %%%%%%%%%%%%%%

post_headers() ->
  [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}|auth_headers()].

auth_headers() ->
  [{<<"AUTHORIZATION">>, iolist_to_binary("Token token="++ulitos_app:get_var(erl_api_client, http_auth_token))}].

compile_path(Path, Options) ->
  compile_path(compile_path(Path), Options, Options, <<"">>).

compile_path(Path) ->
  {ok, RE} = re:compile("(?:^|\/)(\:[^\/]+)"),
  Parts = re:split(Path, RE),
  CompileFun =
    fun
      (<<"">>, Acc) -> Acc;
      (<<":", Var/binary>>, Acc) -> [binary_to_atom(Var, utf8) | Acc];
      (Smth, Acc) -> [Smth | Acc]
    end,
  lists:reverse(lists:foldl(CompileFun, [], Parts)).

compile_path([], _, Params, Acc) ->
  {Acc, Params};

compile_path([Key | Path], Options, Params, Acc) when is_atom(Key) ->
  Value = hackney_bstr:to_binary(proplists:get_value(Key, Options, <<"">>)),
  compile_path(Path, Options, proplists:delete(Key, Params), <<Acc/binary, "/", Value/binary>>);

compile_path([Path | Paths], Options, Params, Acc) ->
  compile_path(Paths, Options, Params, <<Acc/binary, Path/binary>>).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compile_path_test() ->
  ?assertEqual([<<"path/to">>], compile_path("path/to")),
  ?assertEqual([id, <<"/path/to">>], compile_path(":id/path/to")),
  ?assertEqual([id, <<"/path/to">>], compile_path("/:id/path/to")),
  ?assertEqual([<<"smth">>, id, <<"/path/to">>], compile_path("smth/:id/path/to")),
  ?assertEqual({{<<"/1/path/to/1">>, []}}, compile_path("/:id/path/to/:id", [{id, 1}])).

-endif.
