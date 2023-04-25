-module(granderl).

-export([
    init/0,
    uniform/1
]).

-type range() :: 1..4294967295.
-export_type([range/0]).

-compile(no_native).
-on_load(init/0).

-spec init() -> ok.
init() ->
    SoName = filename:join(priv_dir(), "granderl"),
    ok = erlang:load_nif(SoName,[]).

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.

-spec uniform(range()) -> range().
uniform(_N) -> erlang:nif_error(granderl_nif_not_loaded).
