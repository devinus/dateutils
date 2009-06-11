-module(dateutils_tests).

-include_lib("eunit/include/eunit.hrl").

% First I try to parse the different dateformats from 
% http://en.wikipedia.org/wiki/ISO_8601

% 2009-06-10, represented as <Y>-<m>-<d>
simple_date_test() ->
    Parse = dateutils:make_parser("<Y>-<m>-<d>"),
    {{2009,6,10},_} = Parse("2009-06-10").

% Same as above, but without leading zeroes
simple_date2_test() ->
    Parse = dateutils:make_parser("<Y>-<M>-<D>"),
    {{2009,6,10}, _} = Parse("2009-6-10").


% 2009-161, represented as <Y>-<dy>
ordinal_date_test() ->
    Parse = dateutils:make_parser("<Y>-<dy>"),
    io:format("<Y>-<dy>: ~p~n", [Parse("2009-161")]).

% Same as above, without leading zeroes
ordinal_date2_test() ->
    Parse = dateutils:make_parser("<Y>-<DY>"),
    io:format("<Y>-<DY>: ~p~n", [Parse("2008-64")]).

week_and_day_of_week_test() ->
    Parse = dateutils:make_parser("<Y>-W<WY>-<WD>"),
    {{2009,6,11},_} = Parse("2009-W24-4").
