# Erlang Chargify API

## Dependencies

`rebar get-deps` handles all dependencies, but for inquiring minds:

* mochijson2
* ibrowse

## Requirements

The following Erlang applications must be started before using the API

* sasl 
* crypto
* ssl
* ibrowse

## Usage

Please see the documentation for
[Chargify](http://docs.chargify.com/api-introduction) for what it all
this means.

### Example Customer

    Customer = [{<<"first_name">>,<<"Bob">>}, 
       {<<"last_name">>,<<"Wigglesworth">>},
       {<<"email">>,<<"wiggly@example.com">>}].

### Example Product ID
    
    ProductId = {id, 199674}.

### Example Product Handle

    ProductHandle = {handle, <<"widget">>}.

### Example Credit Card
    
    Card = [{<<"full_number">>, <<"1">>}, {<<"expiration_month">>,
    10}}, {<<"expiration_year">>, 2012}].

### Example New Customer

    chargify:save_subscription("subdomain", "yourkey", ProductHandle,
    Customer, Card).

#### Response

     {ok,[{<<"subscription">>,
      [{<<"previous_state">>,<<"active">>},
       {<<"expires_at">>,null},
       {<<"created_at">>,<<"2011-02-18T22:55:48-05:00">>},
       {<<"cancel_at_end_of_period">>,false},
       {<<"activated_at">>,<<"2011-02-18T22:55:50-05:00">>},
       {<<"cancellation_message">>,null},
       {<<"credit_card">>,
        [{<<"customer_vault_token">>,null},
         {<<"vault_token">>,<<"1">>},
         {<<"card_type">>,<<"bogus">>},
         {<<"current_vault">>,<<"bogus">>},
         {<<"expiration_year">>,2023},
         {<<"billing_state">>,null},
         {<<"billing_city">>,null},
         {<<"id">>,199674},
         {<<"billing_address_2">>,null},
         {<<"masked_card_numb"...>>,<<"XXXX-XXXX-XX"...>>},
         {<<"last_name">>,<<"WigglesWorth"...>>},
         {<<"expirati"...>>,10},
         {<<"bill"...>>,null},
         {<<...>>,...},
         {...}|...]},
       {<<"updated_at">>,<<"2011-02-18T22:55:50-05:00">>},
       {<<"trial_ended_at">>,null},
       {<<"current_period_started_at">>,
        <<"2011-02-18T22:55:48-05:00">>},
       {<<"id">>,423194},
       {<<"next_assessment_at">>,<<"2011-03-18T22:55:48-04:00">>},
       {<<"current_period_ends_at">>,
        <<"2011-03-18T22:55:48-04:00">>},
       {<<"customer">>,
        [{<<"reference">>,null},
         {<<"city">>,null},
         {<<"address">>,null},
         {<<"zip">>,null},
         {<<"created_"...>>,<<"2011"...>>},
         {<<"coun"...>>,null},
         {<<...>>,...},
         {...}|...]},
       {<<"signup_revenue">>,<<"2.00">>},
       {<<"signup_payment_id">>,3417013},
       {<<"trial_started_at">>,null},
       {<<"product">>,
        [{<<"name">>,<<"widget">>},
         {<<"retu"...>>,<<>>},
         {<<...>>,...},
         {...}|...]},
       {<<"delayed_cancel_a"...>>,null},
       {<<"balance_in_c"...>>,200},
       {<<"state">>,<<"acti"...>>}]}]}

## Known Issues

If you attempt to start using the `chargify.erl` API from the console
starting like this:

    erl -pa ebin/ deps/*/ebin

Once you make your first call to the API, you will encounter an error
because it cert.pem is not found.

You must start like this if you're testing from the console:

    cd ..
    erl -pa chargify/ebin/ chargify/deps/*/ebin

