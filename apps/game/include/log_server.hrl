
-ifndef(LOG_SERVER_H).
-define(LOG_SERVER_H, true).

-ifdef(TEST).

-define(DEBUG(F), 
        lager:debug(F, [])).
-define(DEBUG(F, D),
        lager:debug(F, D)).


-define(INFO(F), 
        lager:info(F, [])).
-define(INFO(F, D), 
        lager:info(F, D)).

-define(NOTICE(F), 
        lager:notice(F, [])).
-define(NOTICE(F, D), 
        lager:notice(F, D)).

-define(WARN(F), 
        lager:warning(F, [])).
-define(WARN(F, D), 
        lager:warning(F, D)).

-define(ERROR(F), 
        lager:error(F, [])).
-define(ERROR(F, D), 
        lager:error(F, D)).
-define(DB_ERROR(F),
        lager:error(string:concat("DB_ERROR:",F),[])).
-define(DB_ERROR(F,D),
        lager:error(string:concat("DB_ERROR:",F),D)).

-define(ALERT(F), 
        lager:alert(F, [])).
-define(ALERT(F, D), 
        lager:alert(F, D)).

-define(ERROR2(F), 
        lager:alert(F++"\nstacetrace:~p", [erlang:get_stacktrace()])).
-define(ERROR2(F, D), 
        lager:alert(F++"\nstacetrace:~p", D++[erlang:get_stacktrace()])).

-else.

-define(DEBUG(F), 
        util:none(F)).
-define(DEBUG(F, D), 
        util:none(F, D)).

-define(INFO(F), 
        util:none(F)).
-define(INFO(F, D), 
        util:none(F, D)).

-define(NOTICE(F), 
    lager:notice(F, [])).
-define(NOTICE(F, D), 
    lager:notice(F, D)).

-define(WARN(F), 
    lager:warning(F, [])).
-define(WARN(F, D), 
    lager:warning(F, D)).

-define(ERROR(F), 
    lager:error(F, [])).
-define(ERROR(F, D), 
    lager:error(F, D)).

-define(ALERT(F), 
    lager:alert(F, [])).
-define(ALERT(F, D), 
    lager:alert(F, D)).

-define(ERROR2(F), 
    lager:alert(F++"\nstacetrace:~p", [erlang:get_stacktrace()])).
-define(ERROR2(F, D), 
    lager:alert(F++"\nstacetrace:~p", D++[erlang:get_stacktrace()])).

-endif.

-endif.
