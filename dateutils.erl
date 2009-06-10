% Copyright (c) 2009 Jonas Enlund
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
% 
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

-module(dateutils).

-export([make_parser/1, 
	 make_writer/1, 
	 add/3, 
	 add/2,
	 iso8601Week/1
	]).

-export([monday/1, tuesday/1, wednesday/1, thursday/1, friday/1, saturday/1, sunday/1,
	 monday/0, tuesday/0, wednesday/0, thursday/0, friday/0, saturday/0, sunday/0, 
	 today/0, tomorrow/0, yesterday/0
	]).

-include_lib("eunit/include/eunit.hrl").

-define(FULL_YEAR,                           "<Y>").
-define(MONTH_WITH_LEADING_ZEROES,           "<m>").
-define(MONTH_WITHOUT_LEADING_ZEROES,        "<M>").
-define(WEEK_OF_YEAR_WITH_LEADING_ZEROES,    "<wy>").
-define(WEEK_OF_YEAR_WITHOUT_LEADING_ZEROES, "<WY>").
-define(DAY_OF_YEAR_WITH_LEADING_ZEROES,     "<dy>").
-define(DAY_OF_YEAR_WITHOUT_LEADING_ZEROES,  "<DY>").
-define(DAY_OF_MONTH_WITH_LEADING_ZEROES,    "<d>").
-define(DAY_OF_MONTH_WITHOUT_LEADING_ZEROES, "<D>").
-define(DAY_OF_WEEK,                         "<WD>").
-define(HOUR_WITH_LEADING_ZEROES,            "<h>").
-define(HOUR_WITHOUT_LEADING_ZEROES,         "<H>").
-define(MINUTE_WITH_LEADING_ZEROES,          "<mi>").
-define(MINUTE_WITHOUT_LEADING_ZEROES,       "<MI>").
-define(SECOND_WITH_LEADING_ZEROES,          "<s>").
-define(SECOND_WITHOUT_LEADING_ZEROES,       "<S>").



parse(K, digits, Input) ->
    try
	Digits = take(K,Input),
	N = list_to_integer(Digits),
	{ok, N, drop(K,Input)}
    catch
	_:_ -> {error, "no parse"}
    end.

take(0, _)     -> [];
take(N, [H|T]) -> [H|take(N-1, T)].
drop(0, List)  -> List;
drop(N, [_|T]) -> drop(N-1, T).

% A full numeric representation of a year
parse_date(?FULL_YEAR ++ XS, YS, Dict) ->
    {ok, YYYY, Rest} = parse(4, digits, YS),
    parse_date(XS, Rest, dict:append(year, YYYY, Dict));


% Month, with leading zeroes
parse_date(?MONTH_WITH_LEADING_ZEROES ++ XS, YS, Dict) ->
    {ok, MM, Rest} = parse(2, digits, YS),
    parse_date(XS, Rest, dict:append(month, MM, Dict));


% Month, without leading zeroes
parse_date(?MONTH_WITHOUT_LEADING_ZEROES ++ XS, YS, Dict) ->
    case parse(2, digits, YS) of
	{ok, MM, Rest} -> parse_date(XS, Rest, dict:append(month, MM, Dict));
	{error, _}     -> {ok, MM, Rest} = parse(1, digits, YS),
			  parse_date(XS, Rest, dict:append(month, MM, Dict))
    end;

% Week of year, with leading zeroes
parse_date(?WEEK_OF_YEAR_WITH_LEADING_ZEROES ++ XS, YS, Dict) ->
    {ok, WW, Rest} = parse(2, digits, YS),
    parse_date(XS, Rest, dict:append(week_of_year, WW, Dict));

% Week of year, without leading zeroes
parse_date(?WEEK_OF_YEAR_WITHOUT_LEADING_ZEROES ++ XS, YS, Dict) ->
    case parse(2, digits, YS) of
	{ok, WW, Rest} -> parse_date(XS, Rest, dict:append(week_of_year, WW, Dict));
	{error, _}     -> {ok, WW, Rest} = parse(1, digits, YS),
			  parse_date(XS, Rest, dict:append(week_of_year, WW, Dict))
    end;

% Day of year, with leading zeroes
parse_date(?DAY_OF_YEAR_WITH_LEADING_ZEROES ++ XS, YS, Dict) ->
    {ok, DOY, Rest} = parse(3, digits, YS),
    parse_date(XS, Rest, dict:append(day_of_year, DOY, Dict));

% Day of year, without leading zeroes
parse_date(?DAY_OF_YEAR_WITHOUT_LEADING_ZEROES ++ XS, YS, Dict) ->
    case parse(3, digits, YS) of
	{ok, DOY, Rest} -> parse_date(XS, Rest, dict:append(day_of_year, DOY, Dict));
	{error, _} -> case parse(2, digits, YS) of
			  {ok, DOY, Rest} -> parse_date(XS, Rest, dict:append(day_of_year, DOY, Dict));
			  {error, _} -> {ok, DOY, Rest} = parse(1, digits, YS),
					parse_date(XS, Rest, dict:append(day_of_year, DOY, Dict))
		      end
    end;

% Day of the month, with leading zeroes
parse_date(?DAY_OF_MONTH_WITH_LEADING_ZEROES ++ XS, YS, Dict) ->
    {ok, DD, Rest} = parse(2, digits, YS),
    parse_date(XS, Rest, dict:append(day_of_month, DD, Dict));


% Day of the month, without leading zeroes
parse_date(?DAY_OF_MONTH_WITHOUT_LEADING_ZEROES ++ XS, YS, Dict) ->
    case parse(2, digits, YS) of
	{ok, DD, Rest} -> parse_date(XS, Rest, dict:append(day_of_month, DD, Dict));
	{error, _}     -> {ok, DD, Rest} = parse(1, digits, YS),
			  parse_date(XS, Rest, dict:append(day_of_month, DD, Dict))
    end;

% Day of week, 1 digit
parse_date(?DAY_OF_WEEK ++ XS, YS, Dict) ->
    {ok, DW, Rest} = parse(1, digits, YS),
    parse_date(XS, Rest, dict:append(day_of_week, DW, Dict));

% Literal characters must be identical. 
parse_date([X|XS], [X|YS], Dict) ->
    parse_date(XS, YS, Dict);

% End of input.
parse_date([], [], Dict) ->
    Dict.


build_date(DateDict, Defaults) ->
    
    % The year is built first. there can be
    % no conflict here.
    Year = case dict:find(year, DateDict) of
	       {ok, [Y|_]} -> Y;
	       error -> apply(dict:fetch(year, Defaults), [])
	   end,

    % If the day of year is in the DateDict, we use it and we are done.
    case dict:find(day_of_year, DateDict) of
	{ok, [DOY|_]} -> {Date, _} = add({{Year,1,1},{12,0,0}}, DOY-1),
			 Date;
	error ->
	    Month = case dict:find(month, DateDict) of
			{ok, [M|_]} -> M;
			error -> apply(dict:fetch(month, Defaults), [])
		    end,
    
	    Day = case dict:find(day_of_month, DateDict) of
		      {ok, [D|_]} -> D;
		      error -> apply(dict:fetch(day_of_month, Defaults), [])
		  end,
	    {Year, Month, Day}
    end.

build_time(DateDict, Defaults) ->
    Hour = apply(dict:fetch(hour, Defaults), []),	    
    Minute = apply(dict:fetch(minute, Defaults), []),
    Second = apply(dict:fetch(second, Defaults), []),
    {Hour,Minute,Second}.

build_datetime(DateDict, Defaults) ->
    Date = build_date(DateDict, Defaults),
    Time = build_time(DateDict, Defaults),
    {Date, Time}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% make_parser/1 returns a parser that parses dates
%% according to the specified format.
%% 
%% Example:
%% > Parse = dateutils:make_parser(Y/M/D).
%% > Parse("2009/12/24").
%% {2009,12,24}
%% > Parse("2009-12-24").
%% noparse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_parser(Format, Defaults) ->
    fun(DateString) ->
	    DateDict = parse_date(Format, DateString, dict:new()),
	    build_datetime(DateDict, Defaults)
    end.

make_parser(Format) ->
    Defaults = dict:from_list(
		 [{year, fun() -> year(today()) end},
		  {week_of_year, fun() -> 1 end},
		  {month, fun() -> month(today()) end},
		  {day_of_year, fun() -> 1 end},
		  {day_of_month, fun() -> day(today()) end}, % How to fix this?
		  {day_of_week, fun() -> 1 end},
		  {hour, fun() -> 12 end},
		  {minute, fun() -> 0 end},
		  {second, fun() -> 0 end}
		 ]),
    make_parser(Format, Defaults).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% make_writer/1 creates a printer which prints dates
%% according to the specified format.
%%
%% Example
%% > ToString = dateutils:make_writer("D.M Y").
%% > ToString(date()).
%% 5.6 2009
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_writer(Format) ->
    fun(Date) ->
	    to_string(Format, Date)
    end.


to_string(?FULL_YEAR ++ XS, {YYYY, MM, DD}) ->
    integer_to_list(YYYY) ++ to_string(XS, {YYYY, MM, DD});
to_string(?MONTH_WITHOUT_LEADING_ZEROES ++ XS, {YYYY, MM, DD}) ->
    integer_to_list(MM) ++ to_string(XS, {YYYY, MM, DD});
to_string(?DAY_OF_MONTH_WITHOUT_LEADING_ZEROES ++ XS, {YYYY, MM, DD}) ->
    integer_to_list(DD) ++ to_string(XS, {YYYY, MM, DD});
to_string([X|XS], Date) ->
    [X|to_string(XS, Date)];
to_string([], _) ->
    [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add/3 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add(DateTime, N, seconds) ->
    T1 = calendar:datetime_to_gregorian_seconds(DateTime),
    T2 = T1 + N,
    calendar:gregorian_seconds_to_datetime(T2);

add(DateTime, N, minutes) ->
    add(DateTime, 60*N, seconds);

add(DateTime, N, hours) ->
    add(DateTime, 60*N, minutes);

add(DateTime, N, days) ->
    add(DateTime, 24*N, hours);

add(DateTime, N, weeks) ->
    add(DateTime, 7*N, days);

%% 
% Adding months is a bit tricky.
% 
add({{YYYY, MM, DD}=Date, Time}, 0, months) ->
    case calendar:valid_date(Date) of
	true  -> {Date, Time};
	false -> add({{YYYY, MM, DD-1}, Time}, 0, months) % Oops, too many days in this month,
                                                          % Remove a day and try again.
    end;

add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM < 12 ->
    add({{YYYY, MM+1, DD}, Time}, N-1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM =:= 12 ->
    add({{YYYY+1, 1, DD}, Time}, N-1, months); 
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM > 1 ->
    add({{YYYY, MM-1, DD}, Time}, N+1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM =:= 1 ->
    add({{YYYY-1, 12, DD}, Time}, N+1, months);

add(Date, N, years) ->
    add(Date, 12*N, months).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add(Date, second) ->
    add(Date, 1, seconds);
add(Date, minute) ->
    add(Date, 1, minutes);
add(Date, hour) ->
    add(Date, 1, hours);
add(Date, day) ->
    add(Date, 1);
add(Date, week) ->
    add(Date, 1, weeks);
add(Date, month) ->
    add(Date, 1, months);
add(Date, year) ->
    add(Date, 1, years);
add(Date, N)  ->
    add(Date, N, days).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% iso8601Week/1
%%
%% Hopefully according to the iso 8601 standard
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iso8601Week(DateTime) ->
    {{YYYY, _, _} = Thursday, _} = thursday(DateTime),
    Days = calendar:date_to_gregorian_days(Thursday) - calendar:date_to_gregorian_days({YYYY, 1, 1}),
    Days div 7 + 1.

year ({{YYYY, _, _},_}) -> YYYY.
month({{   _,MM, _},_}) -> MM.
day  ({{   _, _,DD},_}) -> DD.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
monday   ({Date,Time}) -> add({Date,Time}, 1 - calendar:day_of_the_week(Date)).
tuesday  ({Date,Time}) -> add({Date,Time}, 2 - calendar:day_of_the_week(Date)).
wednesday({Date,Time}) -> add({Date,Time}, 3 - calendar:day_of_the_week(Date)).
thursday ({Date,Time}) -> add({Date,Time}, 4 - calendar:day_of_the_week(Date)).
friday   ({Date,Time}) -> add({Date,Time}, 5 - calendar:day_of_the_week(Date)).
saturday ({Date,Time}) -> add({Date,Time}, 6 - calendar:day_of_the_week(Date)).
sunday   ({Date,Time}) -> add({Date,Time}, 7 - calendar:day_of_the_week(Date)).

monday   () -> monday   (erlang:localtime()).
tuesday  () -> tuesday  (erlang:localtime()).
wednesday() -> wednesday(erlang:localtime()).
thursday () -> thursday (erlang:localtime()).
friday   () -> friday   (erlang:localtime()).
saturday () -> saturday (erlang:localtime()).
sunday   () -> sunday   (erlang:localtime()).

today    () -> erlang:localtime().
tomorrow () -> add(today(), 1).
yesterday() -> add(today(), -1).
