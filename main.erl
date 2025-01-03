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

get_unit_type(Unit) ->
  case maps:is_key(Unit, ?BINARY_SIZE) of
    true ->
      binaryUnit;
    false ->
      case maps:is_key(Unit, ?DECIMAL_SIZE) of
        true ->
          decimalUnit;
        false ->
          notKnownUnit
      end
  end.

get_constants_from_unit_type(UnitType) ->
  case UnitType of
    binaryUnit ->
      {?BINARY_SIZE, ?BINARY_SIZE_CONSTANT};
    decimalUnit ->
      {?DECIMAL_SIZE, ?DECIMAL_SIZE_CONSTANT};
    _ ->
      notKnownUnit
  end.

size_converter(From, ToUnit) ->
  {FromUnit, Value} = From,
  FromUnitType = get_unit_type(FromUnit),
  {UnitSize, UnitMap} = get_constants_from_unit_type(FromUnitType),
  ToUnitType = get_unit_type(ToUnit),
  if FromUnitType /= ToUnitType ->
       throw(not_same_units_cannot_convert);
     true ->
       ok
  end,

  Magnitudes =
    [case maps:find(X, UnitSize) of
       {ok, Magnitude} ->
         Magnitude;
       _ ->
         throw(wrong_unit)
     end
     || X <- [ToUnit, FromUnit]],

  {ToUnit, Value / math:pow(UnitMap, lists:nth(1, Magnitudes) - lists:nth(2, Magnitudes))}.

enable() ->
  {ok, _} = application:ensure_all_started(os_mon),
  disksup:get_disk_info().
