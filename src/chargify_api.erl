-module(chargify_api).
-export([get/3,
         delete/3,
         delete/4,
         post/4,
         put/4]).
 
%-compile(export_all).
         
get(Account, Key, Path) ->
  request(Account, Key, Path, get, []).

delete(Account, Key, Path) ->
  request(Account, Key, Path, delete, []).
  
delete(Account, Key, Path, Term) ->
  request(Account, Key, Path, delete, Term).

post(Account, Key, Path, Term) ->
  request(Account, Key, Path, post, Term).

put(Account, Key, Path, Term) ->
  request(Account, Key, Path, put, Term).


%% Internal API
request(Account, Key, Path, Method, Term) 
  when is_list(Account), is_list(Key), is_atom(Method), is_list(Term) ->
    CaCertFile = filename:join(code:priv_dir(chargify), "cacert.pem"),
    Options = [{basic_auth, {Key ,"x"}}, {is_ssl, true}, {ssl_options, 
              [{verify, verify_type()}, {cacertfile, CaCertFile}]}],
    Struct = struct(Term),
    JSON = mochijson2:encode(Struct),
    URL = "https://"++Account++".chargify.com"++Path,
    Headers = [{"Content-Type", "application/json"}, {"Accept", "application/json"}],
    case ibrowse:send_req(URL, Headers, Method, JSON, Options, infinity) of
      {ok, Status, _ResponseHeaders, ResponseBody} ->
        case Status of
          Valid when Valid =:= "201"; Valid =:= "200" ->
            %% HACK: leave me alone, this is good enough for now.
            %% return term() deconstructed from JSON
            Result = mochijson2:decode(ResponseBody),
            {ok, destruct(Result)};
          _ -> {error ,[{status, Status}, {body, ResponseBody}]}
        end;
      {error, Reason} -> {error, Reason}
    end.

struct(Elem, Acc) ->
  case Elem of 
    {Key, [{_K,_V} | _T]} ->
      {Key, List} = Elem,
      NestedList = lists:foldl(fun struct/2, [], List),
      Acc ++ [{Key, {struct, NestedList}}];
    _ ->
      Acc ++ [Elem]
  end.
struct(Term) ->
  case Term of 
    {_,_}-> {struct, lists:foldl(fun struct/2, [], Term)};
    [{_,_}|_] -> {struct, lists:foldl(fun struct/2, [], Term)};
    _ -> Term
  end.

destruct(Elem, Acc) ->
  case Elem of 
    {Key, {Struct, List}} when Struct =:= struct; Struct =:= array ->
      NestedList = lists:foldl(fun destruct/2, [], List),
      Acc ++ [{Key, NestedList}];
    {Struct, List} when Struct =:= struct; Struct =:= array ->
      NestedList = lists:foldl(fun destruct/2, [], List),
      Acc ++ NestedList;
    _ ->
      Acc ++ [Elem]
  end.
destruct(Struct) ->
  case Struct of 
    {Type, List} when Type =:= struct; Type =:= array ->
      {Type, Term} = {Type, lists:foldl(fun destruct/2,[], List)};
    [_|_] -> Term = lists:foldl(fun destruct/2,[], Struct);
    _ -> Term = Struct
  end,
  Term.
  
verify_type() ->
  {ok, {V,_,_}} = ssl:version(),
  [Result | _ ] = string:tokens(V, "."),
  Version = list_to_integer(Result),
  case Version > 3 of 
    true -> verify_peer;
    false -> 2
  end.

  
