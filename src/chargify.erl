-module(chargify).
-compile(export_all).


%% Return the first 2000 subscriptions available for the account
%% Account = Key = string()
list_subscriptions(Account, Key) ->
  Path = "/subscriptions.json",
  chargify_api:get(Account, Key, Path).


%% Return N subscriptions per "Page" for the account where N is PerPageStr
%% Account = Key = Page = PerPageStr = string()
list_subscriptions(Account, Key, Page, PerPage) when is_integer(Page), is_integer(PerPage) ->
  PageStr = integer_to_list(Page),
  PerPageStr = integer_to_list(PerPage),
  Path = "/subscriptions.json" ++ "?page=" ++ PageStr ++ "&per_page=" ++ PerPageStr,
  chargify_api:get(Account, Key, Path).

%% Return the subscriptions for a customer by id
%% Account = Key = CustomerID = string()
customer_subscriptions(Account, Key, CustomerId) when is_list(CustomerId) ->
  Path = "/customers/"++CustomerId++"/subscriptions"++".json",
  chargify_api:get(Account, Key, Path).

%% Return the subscription specified by Id
%% Account = Key = Id = string()
get_subscription(Account, Key, Id) when is_list(Id) ->
  Path = "/subscriptions/" ++ Id ++ ".json",
  chargify_api:get(Account, Key, Path).

%% Create a new Subscription with Customer and CreditCard objects
%% Account :: string()
%% Key :: string()
%% Product :: {handle, binary()} | {id, integer()}
%% Customer :: {reference, binary()} |
%%             {id, integer()} |
%%             {attributes, [{<<"first_name">>,binary()},
%%                           {<<"last_name">>, binary()},
%%                           {<<"email">>, binary()}]}]
%% CreditCard :: [{<<"full_number">>, integer()},
%%                {<<"expiration_month">>, integer()},
%%                {<<"expiration_year">>, integer()}]
save_subscription(Account, Key, Product, Customer, CreditCard) ->
  save_subscription(Account, Key, Product, Customer, CreditCard, undefined, undefined).

%% Coupon :: binary() | undefined
%% Component :: [[{<<"component_id">>, integer()}, {<<"allocated_quantity">>, integer()}]] | undefined
save_subscription(Account, Key, Product, Customer, CreditCard, Coupon, Components)
  when is_tuple(Product), is_tuple(Customer), is_list(CreditCard) ->
    Path = "/subscriptions.json",
    Product1 = case Product of
                   {handle, Handle} ->
                       [{<<"product_handle">>, Handle}];
                   {id, PId} ->
                       [{<<"product_id">>, PId}]
               end,
    Customer1 = case Customer of
                    {reference, CRef} ->
                        [{<<"customer_reference">>, CRef}];
                    {id, CId} ->
                        [{<<"customer_id">>, CId}];
                    {attributes, CAttr} ->
                        [{<<"customer_attributes">>, CAttr}]
                end,
    Payment = [{<<"payment_profile_attributes">>, CreditCard}],
    Coupon1 = case Coupon of
                  undefined ->
                      [];
                  _ ->
                      [{<<"coupon_code">>, Coupon}]
              end,
    Components1 = case Components of
                     undefined ->
                         [];
                     _ ->
                         [{<<"components">>, Components}]
                  end,
    Subscription = Product1 ++ Customer1 ++ Payment ++ Coupon1 ++ Components1,
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
    CustomerAttr = case Customer =/= [] of
                       true -> [{<<"customer_attributes">>, Customer}];
                       false -> []
                   end,
    CreditAttr = case CreditCard =/= [] of
                     true -> [{<<"credit_card_attributes">>, CreditCard}];
                     false -> []
                 end,
    Product = case ProductHandle =/= [] of
                  true -> [{<<"product_handle">>, ProductHandle}];
                  false -> []
              end,
    Update =  [{<<"subscription">>, CreditAttr ++ CustomerAttr ++ Product}],
    chargify_api:put(Account, Key, Path, Update).

cancel_subscription(Account, Key, SubscriptionId, CancelMsg)
  when is_list(SubscriptionId), is_binary(CancelMsg) ->
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
  when is_list(SubscriptionId), is_binary(PaymentId), is_binary(Amount), is_binary(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/refunds.json",
    B = [{<<"payment_id">>, PaymentId}],
    B1 = [{<<"amount">>, Amount}] ++ B,
    B2 = [{<<"memo">>, Memo}] ++ B1,
    chargify_api:post(Account, Key, Path, B2).

refund_cents(Account, Key, SubscriptionId, PaymentId, Cents, Memo)
  when is_list(SubscriptionId), is_binary(PaymentId), is_binary(Cents), is_binary(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/refunds.json",
    B = [{<<"payment_id">>, PaymentId}],
    B1 = [{<<"amount_in_cents">>, Cents}] ++ B,
    B2 = [{<<"memo">>, Memo}] ++ B1,
    chargify_api:post(Account, Key, Path, B2).

charge(Account, Key, SubscriptionId, Amount, Memo)
  when is_list(SubscriptionId), is_binary(Amount), is_binary(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/charges.json",
    Charge = [{<<"charge">>, [{<<"amount">>, Amount}, {<<"memo">>, Memo}]}],
    chargify_api:post(Account, Key, Path, Charge).

charge_cents(Account, Key, SubscriptionId, Cents, Memo)
  when is_list(SubscriptionId), is_integer(Cents), is_binary(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/charges.json",
    Charge = [{<<"charge">>, [{<<"amount_in_cents">>, Cents}, {<<"memo">>, Memo}]}],
    chargify_api:post(Account, Key, Path, Charge).

adjust(Account, Key, SubscriptionId, Amount, Memo)
  when is_list(SubscriptionId), is_binary(Amount), is_binary(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/adjustments.json",
    Charge = [{<<"adjustment">>, [{<<"amount">>, Amount}, {<<"memo">>, Memo}]}],
    chargify_api:post(Account, Key, Path, Charge).

adjust_cents(Account, Key, SubscriptionId, Cents, Memo)
  when is_list(SubscriptionId), is_integer(Cents), is_binary(Memo) ->
    Path = "/subscriptions/" ++ SubscriptionId ++ "/adjustments.json",
    Charge = [{<<"adjustment">>, [{<<"amount_in_cents">>, Cents}, {<<"memo">>, Memo}]}],
    chargify_api:post(Account, Key, Path, Charge).

migrate(Account, Key, SubscriptionId, Product)
  when is_list(SubscriptionId), is_tuple(Product) ->
    Path = "/subscriptions/" ++ SubscriptionId ++"/migrations.json",
    Migration = case Product of
      {handle, Handle} -> [{<<"product_handle">>, Handle}];
      {id, Id} -> [{<<"product_id">>, Id}]
    end,
    chargify_api:post(Account, Key, Path, [{<<"migration">>, Migration}]).

transactions(Account, Key) ->
  Path = "/transactions.json",
  chargify_api:get(Account, Key, Path).

list_products(Account, Key) ->
  chargify_api:get(Account, Key, "/products.json").

get_product(Account, Key, Lookup) when is_tuple(Lookup) ->
  Path = case Lookup of
    {ref, Handle} -> "/products/handle/" ++ Handle ++ ".json";
    {id, Id} -> "/products/" ++ Id ++ ".json"
  end,
  chargify_api:get(Account, Key, Path).

list_customers(Account, Key) ->
  chargify_api:get(Account, Key, "/customers.json").

get_customer(Account, Key, Lookup) when is_tuple(Lookup) ->
  Path = case Lookup of
    {ref, Ref} -> "/customers/lookup.json?reference=" ++ Ref;
    {id, Id} -> "/customers/" ++ Id ++ ".json"
  end,
  chargify_api:get(Account, Key, Path).

save_customer(Account, Key, Customer) when is_binary(Customer) ->
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
  when is_list(SubscriptionId), is_list(ComponentId), is_integer(Qty), is_binary(Memo) ->
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
