-module(chargify_cust).
-compile(export_all).

-define(BASEPATH,"/customers").

list(Account, Key) ->
  chargify_req:get(Account, Key, ?BASEPATH ++ ".json").
  
list_subscriptions(Account, Key, Id) ->
  chargify_sub:list_by_customer(Account, Key, Id).
  
lookup_by_id(Account, Key, Id) when is_list(Id) ->
  Path = ?BASEPATH ++ "/" ++ Id ++ ".json",
  chargify_req:get(Account, Key, Path).
  
lookup_by_ref(Account, Key, Ref) when is_list(Ref) ->
  Path = ?BASEPATH ++ "/lookup.json" "?reference=" ++ Ref,
  chargify_req:get(Account, Key, Path).
  
create(Account, Key, Customer) when is_list(Customer) ->
  Prohibited = <<"id">>,
  Required = [<<"first_name">>, <<"last_name">>, <<"email">>],
  Result = lists:foldl(fun(E, L) -> L ++ [lists:keymember(E,1,Customer)] end, [], Required),
  case lists:member(false, Result) of
    true -> {error, require_field_not_found};
    false ->
      case lists:keymember(Prohibited,1,Customer) of
        true -> {error, create_cannot_contain_id};
        false -> 
          Path = ?BASEPATH ++ ".json",
          chargify_req:post(Account, Key, Path, [{<<"customer">>, Customer}])
      end
  end.
  
update(Account, Key, Customer) when is_list(Customer) ->
  case lists:keyfind(<<"id">>,1,Customer) of
    false -> {error, update_requires_id};
    {<<"id">>, Id} ->
      Path = ?BASEPATH ++ "/" ++ Id ++ ".json",  
      chargify_req:put(Account, Key, Path, [{<<"customer">>, Customer}])
  end.