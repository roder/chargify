-module(chargify).
-compile(export_all).

list_subscriptions(Account, Key) ->
  Path = "/subscriptions.json",
  chargify_api:get(Account, Key, Path).
  
list_subscriptions(Account, Key, Page, PerPage) when is_integer(Page), is_integer(PerPage) ->
  PageStr = integer_to_list(Page),
  PerPageStr = integer_to_list(PerPage),
  Path = "/subscriptions.json" ++ "?page=" ++ PageStr ++ "&per_page=" ++ PerPageStr,
  chargify_api:get(Account, Key, Path).

customer_subscriptions(Account, Key, CustomerId) when is_list(CustomerId) ->
  Path = "/customers/"++CustomerId++"/subscriptions"++".json",
  chargify_api:get(Account, Key, Path).
  
get_subscription(Account, Key, Id) when is_list(Id) ->
  Path = "/subscriptions/" ++ Id ++ ".json",
  chargify_api:get(Account, Key, Path).

save_subscription(Account, Key, Product, Customer, CreditCard) ->
  save_subscription(Account, Key, Product, Customer, CreditCard, [], []).

save_subscription(Account, Key, Product, Customer, CreditCard, Coupon, Component) 
  when is_tuple(Product), is_list(Customer), is_list(CreditCard), is_list(Coupon), is_list(Component) ->
    Path = "/subscriptions.json",
    case lists:keyfind(<<"id">>, 1, Customer) of 
      {<<"id">>, Id} -> 
        CustomerAttributes = lists:keydelete(<<"id">>, 1, Customer),
        S = [{<<"customer_id">>, Id}] ++ [{<<"customer_attributes">>, CustomerAttributes}];
      false -> 
        S = [{<<"customer_attributes">>, Customer}]
    end,
    S1 = S ++ [{<<"credit_card_attributes">>, CreditCard}],
    case Coupon =/= [] of 
      true -> S2 = S1 ++ [{<<"coupon_code">>, Coupon}];
      false -> S2 = S1
    end,
    Required = [<<"coupon_id">>, <<"allocated_quantity">>],
    Result = lists:foldl(fun(E, L) -> L ++ [lists:keymember(E,1,Component)] end, [], Required),
    case lists:member(false, Result) of
      true -> S3 = S2;
      false -> S3 = S2 ++ [{<<"components">>, Component}]
    end,
    case Product of 
      {handle, Handle} -> Subscription = S3 ++ [{<<"product_handle">>, Handle}];
      {id, ProdId} -> Subscription = S3 ++ [{<<"product_id">>, ProdId}]
    end,
    chargify_api:post(Account, Key, Path, [{<<"subscription">>, Subscription}]).
    
update_product_subsciption(Account, Key, SubscriptionId, ProductHandle) ->
  update_subscription(Account, Key, SubscriptionId, ProductHandle, [], []).

update_customer_subscription(Account, Key, SubscriptionId, Customer) ->
  update_subscription(Account, Key, SubscriptionId, [], Customer, []).
  
update_creditcard_subscription(Account, Key, SubscriptionId, CreditCard) ->
  update_subscription(Account, Key, SubscriptionId, [], [], CreditCard).

update_subscription(Account, Key, SubscriptionId, ProductHandle, Customer, CreditCard) 
  when is_list(SubscriptionId), is_list(ProductHandle), is_list(Customer), is_list(CreditCard) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ ".json",
    case Customer =/= [] of
      true -> CustomerAttr = [{<<"customer_attributes">>, Customer}];
      false -> CustomerAttr = []
    end, 
    case CreditCard =/= [] of 
      true -> CreditAttr = [{<<"credit_card_attributes">>, CreditCard}];
      false -> CreditAttr = []
    end,
    case ProductHandle =/= [] of 
      true -> Product = [{<<"product_handle">>, ProductHandle}];
      false -> Product = []
    end,
    Update =  [{<<"subscription">>, CreditAttr ++ CustomerAttr ++ Product}],
    chargify_api:put(Account, Key, Path, Update).
    
cancel_subscription(Account, Key, SubscriptionId, CancelMsg) 
  when is_list(SubscriptionId), is_list(CancelMsg) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ ".json",
    Cancel = [{<<"subscription">>, [{<<"cancellation_message">>, CancelMsg}]}],
    chargify_api:delete(Account, Key, Path, Cancel).
    
subscription_transactions(Account, Key, SubscriptionId) when is_list(SubscriptionId) ->
  Path = "/subscriptions/"++SubscriptionId++"/transactions.json",
  chargify_api:get(Account, Key, Path).
  
update_component_qty(Account, Key, SubscriptionId, ComponentId, Qty) 
  when is_list(SubscriptionId), is_list(ComponentId), is_integer(Qty) ->
  Path = "/subscriptions/"++SubscriptionId++"/components/"++ComponentId++".json",
  chargify_api:put(Account, Key, Path, [{<<"component">>, [{<<"allocated_quantity">>, Qty}]}]).

reactivate_subscription(Account, Key, SubscriptionId) when is_list(SubscriptionId) ->
  Path = "/subscriptions/" ++ SubscriptionId ++ "/reactivate.json",
  chargify_api:put(Account, Key, Path, []).  

reset(Account, Key, SubscriptionId) when is_list(SubscriptionId) ->
  Path = "/subscriptions/" ++ SubscriptionId ++ "/reset_balance.json",
  chargify_api:put(Account, Key, Path, []). 
  
refund(Account, Key, SubscriptionId, PaymentId, Amount, Memo) 
  when is_list(SubscriptionId), is_list(PaymentId), is_list(Amount), is_list(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/refunds.json",
    B = [{<<"payment_id">>, PaymentId}],
    B1 = [{<<"amount">>, Amount}] ++ B,
    B2 = [{<<"memo">>, Memo}] ++ B1,
    chargify_api:post(Account, Key, Path, B2).

refund_cents(Account, Key, SubscriptionId, PaymentId, Cents, Memo) 
  when is_list(SubscriptionId), is_list(PaymentId), is_integer(Cents), is_list(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/refunds.json",
    B = [{<<"payment_id">>, PaymentId}],
    B1 = [{<<"amount_in_cents">>, Cents}] ++ B,
    B2 = [{<<"memo">>, Memo}] ++ B1,
    chargify_api:post(Account, Key, Path, B2).
  
charge(Account, Key, SubscriptionId, Amount, Memo)
  when is_list(SubscriptionId), is_list(Amount), is_list(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/charges.json",
    Charge = [{<<"charge">>, [{<<"amount">>, Amount}, {<<"memo">>, Memo}]}],
    chargify_api:post(Account, Key, Path, Charge).
    
charge_cents(Account, Key, SubscriptionId, Cents, Memo)
  when is_list(SubscriptionId), is_integer(Cents), is_list(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/charges.json",
    Charge = [{<<"charge">>, [{<<"amount_in_cents">>, Cents}, {<<"memo">>, Memo}]}],
    chargify_api:post(Account, Key, Path, Charge).

adjust(Account, Key, SubscriptionId, Amount, Memo)
  when is_list(SubscriptionId), is_list(Amount), is_list(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/adjustments.json",
    Charge = [{<<"adjustment">>, [{<<"amount">>, Amount}, {<<"memo">>, Memo}]}],
    chargify_api:post(Account, Key, Path, Charge).

adjust_cents(Account, Key, SubscriptionId, Cents, Memo)
  when is_list(SubscriptionId), is_integer(Cents), is_list(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/adjustments.json",
    Charge = [{<<"adjustment">>, [{<<"amount_in_cents">>, Cents}, {<<"memo">>, Memo}]}],
    chargify_api:post(Account, Key, Path, Charge).
    
migrate(Account, Key, SubscriptionId, Product) 
  when is_list(SubscriptionId), is_tuple(Product) ->
    Path = "/subscriptions/" ++ SubscriptionId ++"/migrations.json",
    case Product of 
      {handle, Handle} -> Migration = [{<<"product_handle">>, Handle}];
      {id, Id} -> Migration = [{<<"product_id">>, Id}]
    end,
    chargify_api:post(Account, Key, Path, [{<<"migration">>, Migration}]).

transactions(Account, Key) ->
  Path = "/transactions.json",
  chargify_api:get(Account, Key, Path).
  
list_products(Account, Key) ->
  chargify_api:get(Account, Key, "/products.json").
  
get_product(Account, Key, Lookup) when is_tuple(Lookup) ->
  case Lookup of 
    {ref, Handle} -> Path = "/products/handle/" ++ Handle ++ ".json";
    {id, Id} -> Path = "/products/" ++ Id ++ ".json"
  end,
  chargify_api:get(Account, Key, Path).

list_customers(Account, Key) ->
  chargify_api:get(Account, Key, "/customers.json").

get_customer(Account, Key, Lookup) when is_tuple(Lookup) ->
  case Lookup of 
    {ref, Ref} -> Path = "/customers/lookup.json?reference=" ++ Ref;
    {id, Id} -> Path = "/customers/" ++ Id ++ ".json"
  end,
  chargify_api:get(Account, Key, Path).

save_customer(Account, Key, Customer) when is_list(Customer) ->
  case lists:keyfind(<<"id">>,1,Customer) of
    false -> 
      Required = [<<"first_name">>, <<"last_name">>, <<"email">>],
      Result = lists:foldl(fun(E, L) -> L ++ [lists:keymember(E,1,Customer)] end, [], Required),
      case lists:member(false, Result) of
        true -> {error, require_field_not_found};
        false -> 
          Path = "/customers.json",
          chargify_api:post(Account, Key, Path, [{<<"customer">>, Customer}])
      end;
    {<<"id">>, Id} ->
      %% Update
      Path = "/customers/" ++ Id ++ ".json",
      chargify_api:put(Account, Key, Path, [{<<"customer">>, Customer}]) 
  end.
  
list_components(Account, Key, ProductFamilyId) when is_list(ProductFamilyId) ->
  Path = "/product_families/"++ProductFamilyId++"/components.json",
  chargify_api:get(Account, Key, Path).
  
list_usages(Account, Key, SubscriptionId, ComponentId) 
  when is_list(SubscriptionId), is_list(ComponentId) ->
    Path = "/subscriptions/"++SubscriptionId++"/components/"++ComponentId++"/usages.json",
    chargify_api:get(Account, Key, Path).
    
save_usage(Account, Key, SubscriptionId, ComponentId, Qty, Memo) 
  when is_list(SubscriptionId), is_list(ComponentId), is_integer(Qty), is_list(Memo) ->
    Path = "/subscriptions/"++SubscriptionId++"/components/"++ComponentId++"/usages.json",
    Body = [{<<"usage">>, [{<<"quantity">>, Qty},{<<"memo">>, Memo}]}],
    chargify_api:post(Account, Key, Path, Body).


get_coupon(Account, Key, ProductFamilyId, CouponId) 
  when is_list(ProductFamilyId), is_list(CouponId) ->
    Path = "/product_families/"++ProductFamilyId++"/coupons/"++CouponId++".json",
    chargify_api:get(Account, Key, Path).

find_coupon(Account, Key, ProductFamilyId, CouponCode) 
  when is_list(ProductFamilyId), is_list(CouponCode) ->
    Path = "/product_families/"++ProductFamilyId++"/coupons/find.json?code="++CouponCode,
    chargify_api:get(Account, Key, Path).
