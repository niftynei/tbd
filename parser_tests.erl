-module(parser_tests).
-include_lib("eunit/include/eunit.hrl").

device_test() ->
  {ok, self} = parser:get_device([{device, self}]),
  {ok, <<255, 10, 45, 42>>} = parser:get_device([{device, <<255, 10, 45, 42>>}]),
  {ok, <<255, 10, 45, 42>>} = parser:get_device([{device, "FF0A2D2A"}]),
  {ok, <<255, 10, 45, 42, 255, 10, 45, 42>>} = parser:get_device([{device, 16#FF0A2D2AFF0A2D2A}]),
  {error, device_bad_format} = parser:get_device([{device, {}}]),
  {ok, self} = parser:get_device([{device}]),
  ok.

command_test() ->
  {ok, <<68, 49>>} = parser:get_command([{command, <<"D1">>}]),
  {ok, <<68, 49>>} = parser:get_command([{command, "D1"}]),
  {error, command_bad_format} = parser:get_command([{command, 4242}]),
  {error, command_undefined} = parser:get_command([{command}]),
  ok.

count_test() -> 
  10 = parser:calc_count(<<"ello there">>),
  8 = parser:calc_count(<<10, 100, 9, 19, 6, 8, 55, 100>>),
  5 = parser:calc_count([[<<"hi">>],[<<"one">>]]),
  0 = parser:calc_count([]),
  0 = parser:calc_count(<<>>),
  ok.

