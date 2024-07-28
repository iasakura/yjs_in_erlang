-module(id_set).

-export_type([delete_set/0]).

-type id_range() :: {continuous, range:range()} | {fragmented, [range:range()]}.

-type id_set() :: #{state_vector:client_id() => id_range()}.

-type delete_set() :: id_set().

-spec insert()