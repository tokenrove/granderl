-module(granderl).

-export([
    init/0,
    uniform/1
]).

-on_load(init/0).

-define(SO_NAME, "granderl").

-type range() :: 1..4294967295.

%% public
-spec init() -> ok.

init() ->
    SoName = filename:join(priv_dir(), ?SO_NAME),
    case erlang:load_nif(SoName,[]) of
        ok -> ok;
        {error, Reason} ->
             error_logger:error_msg("~p load_nif error: ~p~n",
                 [?MODULE, Reason])
    end.

-spec uniform(range()) -> range().

uniform(_N) ->
    not_loaded(?LINE).

%% private
priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
