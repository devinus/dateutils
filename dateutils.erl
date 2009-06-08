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

-define(FULL_YEAR, "Y").
-define(DAY_OF_MONTH_WITH_LEADING_ZEROES, "d").
-define(DAY_OF_MONTH_WITHOUT_LEADING_ZEROES, "D").
-define(MONTH_WITH_LEADING_ZEROES, "m").
-define(MONTH_WITHOUT_LEADING_ZEROES, "M").


% A full numeric representation of a year
parse_date(?FULL_YEAR ++ XS, [Y1,Y2,Y3,Y4|YS], {_YYYY, MM, DD}) ->
    parse_date(XS, YS, {list_to_integer([Y1, Y2, Y3, Y4]), MM, DD});


% Day of the month, with leading zeros
parse_date(?DAY_OF_MONTH_WITH_LEADING_ZEROES ++ XS, [Y1,Y2|YS], {YYYY, MM, _DD}) ->
    parse_date(XS, YS, {YYYY, MM, list_to_integer([Y1,Y2])});


% Day of the month, without leading zeroes
parse_date(?DAY_OF_MONTH_WITHOUT_LEADING_ZEROES ++ XS, [Y1,Y2|YS], {YYYY, MM, _DD}) ->
    try parse_date(XS, YS, {YYYY, MM, list_to_integer([Y1,Y2])}) 
    catch
	_:_ -> parse_date(XS, [Y2|YS], {YYYY, MM, list_to_integer([Y1])})
    end;
parse_date(?DAY_OF_MONTH_WITHOUT_LEADING_ZEROES ++ XS, [Y1], {YYYY, MM, _DD}) ->
    parse_date(XS, [], {YYYY, MM, list_to_integer([Y1])});


% Numeric representation of a month, with leading zeros
parse_date(?MONTH_WITH_LEADING_ZEROES ++ XS, [Y1,Y2|YS], {YYYY, _MM, DD}) ->
    parse_date(XS, YS, {YYYY, list_to_integer([Y1, Y2]), DD});


% Numeric representation of a month, without leading zeros
parse_date(?MONTH_WITHOUT_LEADING_ZEROES ++ XS, [Y1, Y2|YS], {YYYY, _MM, DD}) ->
    try parse_date(XS, YS, {YYYY, list_to_integer([Y1, Y2]), DD})
    catch
	_:_ -> parse_date(XS, [Y2|YS], {YYYY, list_to_integer([Y1]), DD})
    end;
parse_date(?MONTH_WITHOUT_LEADING_ZEROES ++ XS, [Y1], {YYYY, _MM, DD}) ->
    parse_date(XS, [], {YYYY, list_to_integer([Y1]), DD});


% Literal characters must be identical. 
parse_date([X|XS], [X|YS], Date) ->
    parse_date(XS, YS, Date);

% End of input.
parse_date([], [], Date) ->
    case calendar:valid_date(Date) of
	true -> Date;
	false -> noparse 
    end.


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
make_parser(Format) ->
    fun(DateString) ->
	    try
		parse_date(Format, DateString, date())
	    catch
		_:_ -> noparse
	    end
    end.


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
