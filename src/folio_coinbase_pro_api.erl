-module(folio_coinbase_pro_api).

-include_lib("kernel/include/logger.hrl").

-behavior(folio_integration).

-export([folio_init/0]).
-export([setup_properties/0, add/2]).
-export([accounts_init/1, accounts/1]).
-export([account_transactions_init/2, account_transactions/1]).

folio_init() ->
    folio_throttle:setup(?MODULE, 5, per_second).

setup_properties() ->
    [
        #{
            fields =>
                #{
                    passphrase => #{},
                    key => #{},
                    secret => #{}
                }
        }
    ].

add(IntegrationID, #{key := K, secret := S, passphrase := P}) ->
    Credentials = #{key => K, secret => S, passphrase => P},
    ok = folio_credentials_store:set_credentials(IntegrationID, Credentials),
    ok.

accounts_init(_Integration = #{id := IntegrationID}) ->
    #{
        integration_id => IntegrationID
    }.

accounts(State) ->
    {ok, _Headers, AccountResp, State1} = request(<<"/accounts">>, State),

    FAccounts = lists:map(fun cb_to_account/1, AccountResp),
    {complete, FAccounts, State1}.

account_transactions_init(#{id := IntegrationID}, #{id := AccountID}) ->
    State = #{
        account_id => AccountID,
        integration_id => IntegrationID,
        currency => undefined,
        next_ledger_qs => <<"">>
    },
    State.

account_transactions(State = #{account_id := AccountID, currency := undefined}) ->
    % The symbol/currency isn't stored in our account (it's part of balance)
    % but the coinbasepro account is 1 currency. Make a request when starting
    % to avoid complicating our API
    Path = <<<<"/accounts/">>/binary, AccountID/binary>>,
    {ok, _Headers, Resp, State1} = request(Path, State),
    #{<<"currency">> := Currency} = Resp,
    State2 = State1#{currency => Currency},
    {incomplete, [], State2};
account_transactions(
    State = #{account_id := AccountID, currency := Currency, next_ledger_qs := QS}
) ->
    Path = <<<<"/accounts/">>/binary, AccountID/binary, <<"/ledger">>/binary, QS/binary>>,
    {ok, Headers, Resp, State1} = request(Path, State),

    CBBefore = proplists:get_value(<<"CB-BEFORE">>, Headers),

    TXLists = lists:map(fun(I) -> cb_to_txs(I, State1) end, Resp),
    TXs = lists:flatten(TXLists),

    {Complete, State2} =
        case CBBefore of
            undefined ->
                {complete, State1};
            _ ->
                NextQS = <<<<"?before=">>/binary, CBBefore/binary>>,
                {incomplete, State1#{next_ledger_qs => NextQS}}
        end,
    ?LOG_DEBUG(#{
        message => coinbase_pro_ledger,
        state => State,
        headers => Headers,
        complete => Complete,
        currency => Currency,
        resp => Resp,
        path => Path
    }),
    % {ok, Resp, State1} = request(NextURI, State),
    % Data = maps:get(<<"data">>, Resp),

    % Pagination = maps:get(<<"pagination">>, Resp, #{}),
    % NextNextURI = maps:get(<<"next_uri">>, Pagination, null),

    % % Determine if this is complete
    % Complete =
    %     case NextNextURI of
    %         null -> complete;
    %         _ -> incomplete
    %     end,
    % FolioTXs = lists:flatten(lists:map(fun cb_to_tx/1, Data)),
    % State2 = State1#{next_uri => NextNextURI},
    % {Complete, FolioTXs, State2}.
    {Complete, TXs, State2}.

cb_to_account(#{
    <<"id">> := SourceID,
    <<"currency">> := Currency,
    <<"balance">> := BalanceS
}) ->
    Balance = folio_math:to_decimal(BalanceS),
    #{
        id => SourceID,
        balances => [
            #{
                balance => Balance,
                symbol => Currency
            }
        ]
    }.

cb_to_txs(
    #{
        <<"created_at">> := CreatedAt,
        <<"id">> := LedgerID,
        <<"amount">> := Amount,
        <<"type">> := <<"transfer">>,
        <<"details">> := #{
            <<"to">> := ToAccountID,
            <<"from">> := FromAccountID
        }
    },
    _State = #{account_id := AccountID, currency := Currency}
) ->
    Direction =
        case AccountID of
            ToAccountID -> in;
            FromAccountID -> out
        end,
    #{
        source_id => LedgerID,
        datetime => qdate:to_date(CreatedAt),
        direction => Direction,
        symbol => Currency,
        amount => folio_math:to_decimal(Amount),
        type => undefined,
        description => <<"sell">>
    };
cb_to_txs(
    #{
        <<"created_at">> := CreatedAt,
        <<"id">> := LedgerID,
        <<"amount">> := Amount,
        <<"type">> := <<"transfer">>,
        <<"details">> := #{
            <<"transfer_type">> := TransferType
        }
    },
    _State = #{currency := Currency}
) ->
    Direction =
        case TransferType of
            <<"deposit">> -> in;
            <<"withdraw">> -> out
        end,
    #{
        source_id => LedgerID,
        datetime => qdate:to_date(CreatedAt),
        direction => Direction,
        symbol => Currency,
        amount => decimal:abs(folio_math:to_decimal(Amount)),
        type => undefined,
        description => TransferType
    };
cb_to_txs(
    #{
        <<"created_at">> := CreatedAt,
        <<"id">> := LedgerID,
        <<"amount">> := Amount,
        <<"type">> := <<"fee">>,
        <<"details">> := #{
            <<"product_id">> := ProductID
        }
    },
    _State = #{currency := Currency}
) ->
    #{
        source_id => LedgerID,
        datetime => qdate:to_date(CreatedAt),
        direction => out,
        symbol => Currency,
        amount => decimal:abs(folio_math:to_decimal(Amount)),
        type => fee,
        description => <<<<"fee ">>/binary, ProductID/binary>>
    };
cb_to_txs(
    #{
        <<"created_at">> := CreatedAt,
        <<"id">> := LedgerID,
        <<"amount">> := AmountF,
        <<"type">> := <<"match">>,
        <<"details">> := #{
            <<"product_id">> := ProductID
        }
    },
    _State = #{currency := Currency}
) ->
    Amount = folio_math:to_decimal(AmountF),
    AmountABS = decimal:abs(Amount),
    Direction =
        case Amount == AmountABS of
            true -> in;
            false -> out
        end,
    #{
        source_id => LedgerID,
        datetime => qdate:to_date(CreatedAt),
        direction => Direction,
        symbol => Currency,
        amount => AmountABS,
        type => undefined,
        description => ProductID
    };
cb_to_txs(
    #{
        <<"created_at">> := CreatedAt,
        <<"id">> := LedgerID,
        <<"amount">> := AmountF,
        <<"type">> := <<"conversion">>
    },
    _State = #{currency := Currency}
) ->
    Amount = folio_math:to_decimal(AmountF),
    AmountABS = decimal:abs(Amount),
    Direction =
        case Amount == AmountABS of
            true -> in;
            false -> out
        end,
    #{
        source_id => LedgerID,
        datetime => qdate:to_date(CreatedAt),
        direction => Direction,
        symbol => Currency,
        amount => AmountABS,
        type => undefined,
        description => <<"stablecoin conversion">>
    }.

credentials(_State = #{integration_id := ID}) ->
    C =
        #{key := _Key, secret := _Secret, passphrase := _Passphrase} = folio_credentials_store:get_credentials(
            ID
        ),
    C.

-spec request(binary(), any()) -> {ok, list(), map() | list(), any()} | {error, binary(), any()}.
request(PathQS, State) ->
    request(PathQS, #{attempts_remaining => 3}, State).

-spec request(binary(), map(), any()) ->
    {ok, list(), map() | list(), any()} | {error, binary(), any()}.
request(PathQS, #{attempts_remaining := AR}, State) when AR =< 0 ->
    ?LOG_INFO(#{
        message => "Coinbase request failed",
        path => PathQS
    }),
    {error, "No more attemps remaining", State};
request(PathQS, Opts = #{attempts_remaining := AR}, State) ->
    BasePath = <<"https://api.exchange.coinbase.com">>,
    Url = <<BasePath/binary, PathQS/binary>>,

    rate_limit(),
    % coinbase_sign requires a timestamp so rate limiting before it can cause
    % signing errors if the request is delayed too long
    Headers = coinbase_sign(get, PathQS, State),

    case hackney:request(get, Url, Headers, [], [with_body]) of
        {error, timeout} ->
            request(PathQS, Opts#{attempts_remaining => AR - 1}, State);
        {ok, _RespCode, RespHeaders, Body} ->
            case jsx:is_json(Body) of
                true -> {ok, RespHeaders, jsx:decode(Body, [return_maps]), State};
                false -> {error, Body, State}
            end
    end.

coinbase_sign(get, Path, State) ->
    Now = os:system_time(second),

    #{key := Key, secret := B64Secret, passphrase := Passphrase} = credentials(State),

    NowBin = erlang:integer_to_binary(Now),

    SigMesg = <<NowBin/binary, <<"GET">>/binary, Path/binary>>,
    Secret = base64:decode(B64Secret),

    SigDigest = crypto:mac(hmac, sha256, Secret, SigMesg),
    Sig = base64:encode(SigDigest),

    Headers = [
        {<<"CB-ACCESS-KEY">>, Key},
        {<<"CB-ACCESS-TIMESTAMP">>, NowBin},
        {<<"CB-ACCESS-PASSPHRASE">>, Passphrase},
        {<<"CB-ACCESS-SIGN">>, Sig},
        {<<"User-Agent">>, <<"folio">>},
        {<<"Content-Type">>, <<"application/json">>}
    ],
    Headers.

rate_limit() ->
    folio_throttle:rate_limit(?MODULE, key).
