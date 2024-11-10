-module(edenticon_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ElliOpts = [{callback, edenticon_web}
               ,{port, 9091}
               ],
    ElliSpec = {edenticon_web
               ,{elli, start_link, [ElliOpts]}
               ,permanent
               ,5000
               ,worker
               ,[elli]
               },
    logger:notice("Starting service at http://127.0.0.1:~p~n", [9091]),
    {ok, {{one_for_one, 5, 10}, [ElliSpec]}}.
