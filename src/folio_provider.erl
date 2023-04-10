-module(folio_provider).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([
    providers/0,
    provider_by_name/1,
    provider_setup_properties/1
]).

-export_type([name/0]).
-type name() :: binary().

-export_type([provider/0]).
-type provider() :: #{
    name := name(),
    type := exchange | chain,
    mod := atom()
}.

-export_type([setup_property_config/0]).
-type setup_property_config() :: #{
    fields := map()
}.

providers() ->
    [
        #{
            name => <<"coinbase">>,
            type => exchange,
            mod => folio_coinbase_api
        },
        #{
            name => <<"coinbase_pro">>,
            type => exchange,
            mod => folio_coinbase_pro_api
        },
        #{
            name => <<"bitcoin">>,
            type => chain,
            mod => folio_blockstream
        },
        #{
            name => <<"gemini">>,
            type => exchange,
            mod => folio_gemini_api
        },
        #{
            name => <<"ethereum">>,
            type => chain,
            mod => folio_ethplorer
        },
        #{
            name => <<"loopring">>,
            type => chain,
            mod => folio_loopring
        }
    ].

-spec provider_by_name(binary()) -> provider().
provider_by_name(Name) ->
    Ints = providers(),
    case lists:search(fun(#{name := N}) -> N == Name end, Ints) of
        {value, V} -> V;
        _ -> throw(not_found)
    end.

-spec provider_setup_properties(binary()) -> map().
provider_setup_properties(Name) ->
    #{mod := Mod} = provider_by_name(Name),
    Props = Mod:setup_properties(),
    Props.
