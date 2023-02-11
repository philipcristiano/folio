-module(folio_data).

-export([create_table/2]).

create_table(Name, ExtraAttr) ->
    Attrs = [{disc_copies, [nodes()]} | ExtraAttr],
    mnesia:create_table(Name, Attrs).
