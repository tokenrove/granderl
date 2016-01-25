-module(granderl).

-export([
    init/0,
    rdrand/1,
    xorshift_tls/1,
    xorshift_yolo/1,
    xorshift_rdtsc/1,
    rdtsc_mod/1
]).

-on_load(init/0).

-spec init() -> ok.
init() ->
    SoName = filename:join(priv_dir(), "granderl_nif"),
    case catch erlang:load_nif(SoName,[]) of
        _ -> ok
    end.

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.

-type range() :: 1..4294967295.

-spec rdrand(range()) -> any().
rdrand(_N) -> erlang:nif_error(granderl_nif_not_loaded).

-spec xorshift_tls(range()) -> any().
xorshift_tls(_N) -> erlang:nif_error(granderl_nif_not_loaded).

-spec xorshift_yolo(range()) -> any().
xorshift_yolo(_N) -> erlang:nif_error(granderl_nif_not_loaded).

-spec xorshift_rdtsc(range()) -> any().
xorshift_rdtsc(_N) -> erlang:nif_error(granderl_nif_not_loaded).

-spec rdtsc_mod(range()) -> any().
rdtsc_mod(_N) -> erlang:nif_error(granderl_nif_not_loaded).
