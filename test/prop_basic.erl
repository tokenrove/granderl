-module(prop_basic).

-include_lib("proper/include/proper.hrl").

prop_bounds() ->
    ?FORALL(N, range(1,4294967295),
            begin
                O = granderl:uniform(N),
                O >= 1 andalso O =< N
            end).
