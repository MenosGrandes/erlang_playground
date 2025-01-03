-module(main).

-export([enable/0]).
-export([size_converter/2]).

-define(BINARY_SIZE_CONSTANT, 1024).
-define(DECIMAL_SIZE_CONSTANT, 1000).
-define(BINARY_SIZE,
        #{kibibyte => 0,
          mebibyte => 1,
          gibibyte => 2,
          tebibyte => 3,
          pebibyte => 4}).
-define(DECIMAL_SIZE,
        #{kilobyte => 0,
          megabyte => 1,
          gigabyte => 2,
          terabyte => 3,
          petabyte => 4}).

size_converter(From, Unit) ->
  {FromUnit, Value} = From,

  Magnitudes =
    [case maps:find(X, ?BINARY_SIZE) of
       {ok, Magnitude} ->
         Magnitude;
       _ ->
         throw(wrong_unit)
     end
     || X <- [Unit, FromUnit]],
  Value
  / math:pow(?BINARY_SIZE_CONSTANT, lists:nth(1, Magnitudes) - lists:nth(2, Magnitudes)).

enable() ->
  {ok, _} = application:ensure_all_started(os_mon),
  disksup:get_disk_info().
