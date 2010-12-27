-module(chargify_sub).
%%-export().'
-compile(export_all).

-define(BASEPATH, "/subscriptions").

list(Account, Key) ->
  Path = ?BASEPATH ++ ".json",
  chargify_req:get(Account, Key, Path).
  
list(Account, Key, Page, PerPage) when is_integer(Page), is_integer(PerPage) ->
  PageStr = integer_to_list(Page),
  PerPageStr = integer_to_list(PerPage),
  Path = ?BASEPATH ++ ".json" ++ "?page=" ++ PageStr ++ "&per_page=" ++ PerPageStr,
  chargify_req:get(Account, Key, Path).

list_by_customer(Account, Key, CustomerId) when is_list(CustomerId) ->
  Path = "/customers/"++CustomerId++?BASEPATH++".json",
  chargify_req:get(Account, Key, Path).
  
read(Account, Key, Id) when is_list(Id) ->
  Path = ?BASEPATH ++ "/" ++ Id ++ ".json",
  chargify_req:get(Account, Key, Path).

create(Account, Key, SubscriptionId, Customer, CreditCard) ->
  create(Account, Key, SubscriptionId, Customer, CreditCard, []).

create(Account, Key, SubscriptionId, Customer, CreditCard, Coupon) 
  when is_list(SubscriptionId), is_list(Customer), is_list(CreditCard), is_list(Coupon) ->
    Path = ?BASEPATH ++ ".json",
    Subscription =  [{<<"subscription">>, [{<<"customer_attributes">>, Customer},
                                            <<"credit_card_attributes">>, CreditCard]}],
    case Coupon =/= [] of 
      true -> Create = Subscription ++ [{<<"coupon_code">>, Coupon}];
      false -> Create = Subscription
    end,
    chargify_req:post(Account, Key, Path, Create).

update(Account, Key, SubscriptionId, Customer, CreditCard) 
  when is_list(SubscriptionId), is_list(Customer), is_list(CreditCard) ->
    Path = ?BASEPATH ++ "/" ++ SubscriptionId ++ ".json",
    case Customer =/= [] of
      true -> CustomerAttr = [{<<"customer_attributes">>, Customer}];
      false -> CustomerAttr = []
    end, 
    case CreditCard =/= [] of 
      true -> CreditAttr = [{<<"credit_card_attributes">>, CreditCard}];
      false -> CreditAttr = []
    end,
    Update =  [{<<"subscription">>, CreditAttr ++ CustomerAttr}],
    chargify_req:put(Account, Key, Path, Update).
    
cancel(Account, Key, SubscriptionId, CancelMsg) 
  when is_list(SubscriptionId), is_list(CancelMsg) ->
    Path = ?BASEPATH ++ "/" ++ SubscriptionId ++ ".json",
    Cancel = [{<<"subscription">>, [{<<"cancellation_message">>, CancelMsg}]}],
    chargify_req:delete(Account, Key, Path, Cancel).
    
reactive(Account, Key, SubscriptionId) when is_list(SubscriptionId) ->
  Path = ?BASEPATH ++ "/" ++ SubscriptionId ++ "/reactivate.json",
  chargify_req:put(Account, Key, Path, []).  

reset(Account, Key, SubscriptionId) when is_list(SubscriptionId) ->
  Path = ?BASEPATH ++ "/" ++ SubscriptionId ++ "/reset_balance.json",
  chargify_req:put(Account, Key, Path, []). 
  
charge(Account, Key, SubscriptionId, Amount, Memo)
  when is_list(SubscriptionId), is_list(Amount), is_list(Memo) ->
    Path = ?BASEPATH ++ "/" ++ SubscriptionId ++ "/charges.json",
    Charge = [{<<"charge">>, [{<<"amount">>, Amount}, {<<"memo">>, Memo}]}],
    chargify_req:post(Account, Key, Path, Charge).
    
charge_cents(Account, Key, SubscriptionId, Cents, Memo)
  when is_list(SubscriptionId), is_integer(Cents), is_list(Memo) ->
    Path = ?BASEPATH ++ "/" ++ SubscriptionId ++ "/charges.json",
    Charge = [{<<"charge">>, [{<<"amount_in_cents">>, Cents}, {<<"memo">>, Memo}]}],
    chargify_req:post(Account, Key, Path, Charge).

adjust(Account, Key, SubscriptionId, Amount, Memo)
  when is_list(SubscriptionId), is_list(Amount), is_list(Memo) ->
    Path = ?BASEPATH ++ "/" ++ SubscriptionId ++ "/adjustments.json",
    Charge = [{<<"adjustment">>, [{<<"amount">>, Amount}, {<<"memo">>, Memo}]}],
    chargify_req:post(Account, Key, Path, Charge).

adjust_cents(Account, Key, SubscriptionId, Cents, Memo)
  when is_list(SubscriptionId), is_integer(Cents), is_list(Memo) ->
    Path = ?BASEPATH ++ "/" ++ SubscriptionId ++ "/adjustments.json",
    Charge = [{<<"adjustment">>, [{<<"amount_in_cents">>, Cents}, {<<"memo">>, Memo}]}],
    chargify_req:post(Account, Key, Path, Charge).

transactions(Account, Key, SubscriptionId) when is_list(SubscriptionId) ->
  Path = ?BASEPATH++"/"++SubscriptionId++"/transactions.json",
  chargify_req:get(Account, Key, Path).   