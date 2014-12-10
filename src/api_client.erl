-module(api_client).

-export([request/4, post/2]).

-include("log.hrl").

request(Method, Headers, Path, Params) ->
  case host() of
    false -> {error, http_unavailable};
    {Host, Options} ->
      case ulitos_app:get_var(api_client_erl, Path) of
        undefined -> {error, path_not_set};
        RecPath -> 
        	case hackney:request(Method, <<Host/binary, RecPath/binary>>, Headers, params_to_payload(Params), Options) of
				    {ok, 200, _, ClientRef} ->
              case hackney:body(ClientRef) of
                {ok, Body} ->
                  {JsonResponse} = jiffy:decode(Body),
                  ?D({user_info, JsonResponse}),
                  {ok, JsonResponse};
                {error, _Reason} ->
                  {error, hackney_error}
              end;
				    {ok, Other, _, ClientRef} -> 
              ?D({http_error,Other, hackney:body(ClientRef)}),
              {error, http_error};
				    {error, Reason} -> 
              ?D({"error calling meeting info handler ~p~n", RecPath, Reason}),
              {error, req_failed}
			    end
      end
  end.

post(Path, Params) ->
  	request(post, post_headers(), Path, Params).

host() ->
  {Prefix, Options} = case ulitos_app:get_var(api_client_erl, http_ssl) of
                        true -> {<<"https://">>, []};
                        false -> {<<"http://">>, []};
                        _ -> false
                      end,
  case {ulitos_app:get_var(api_client_erl, http_host), Prefix} of
    {undefined, _} -> false;
    {_, false} -> false;
    {Host, Prefix} ->
    	{<<Prefix/binary, Host/binary>>, Options}
  end.

  %%%%%%%%% private %%%%%%%%%%%%%%

params_to_payload(Params) ->
	params_to_payload(Params, []).

params_to_payload([], Acc) ->
	list_to_binary(Acc);	

params_to_payload([{Key, Value}], Acc) ->
	list_to_binary([print(Key), "=", print(Value)| Acc]);

params_to_payload([{Key, Value}|Params], Acc) ->
	params_to_payload(Params, ["&", print(Key), "=", print(Value) | Acc]).

print(Term) when is_list(Term) orelse is_binary(Term) ->
	Term;

print(Term) when is_atom(Term) ->
	atom_to_list(Term);

print(Term) ->
	io_lib:format("~p", [Term]).

post_headers() ->
  [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}|auth_headers()].

auth_headers() ->
  [{<<"AUTHORIZATION">>, iolist_to_binary("Token token="++ulitos_app:get_var(api_client_erl, http_auth_token))}].