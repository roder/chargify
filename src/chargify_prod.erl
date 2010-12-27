-module(chargify_prod).
%-export().
-compile(export_all).

-define(BASEPATH, "/products").

list(Account, Key) ->
  chargify_req:get(Account, Key, ?BASEPATH ++ ".json"). 
  
lookup_by_id(Account, Key, Id) when is_list(Id) ->
  Path = ?BASEPATH ++ Id ++ ".json",
  chargify_req:get(Account, Key, Path).
  
lookup_by_handle(Account, Key, Handle) when is_list(Handle) ->
  Path = ?BASEPATH ++ "/handle/" ++ Handle ++ ".json",
  chargify_req:get(Account, Key, Path).
