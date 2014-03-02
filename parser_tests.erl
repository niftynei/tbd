-module(parser_tests).
-include_lib("eunit/include/eunit.hrl").
-include("constants.hrl").

parser_test_() ->
  [test_device(),
  test_binary(),
  test_checksum(),
  test_print()
  ].

test_print() ->
  [?_assertEqual("7e000408014e4464", parser:print_frame(#frame{at_command="ND"})),
  ?_assertEqual("7e000508014e4aff5f", parser:print_frame(#frame{at_command="NJ", value=16#FF}))
  ].

test_device() ->
  [?_assert(parser:validate_device(<<1,2,3,4,5,6,7,8>>)),
  ?_assertError(invalid_device_addr, parser:validate_device(<<>>))
  ].

test_binary() ->
  [?_assertEqual(<<68, 49>>, parser:get_binary(<<"D1">>)),
  ?_assertEqual(<<68, 49>>, parser:get_binary("D1")),
  ?_assertEqual(<<1>>, parser:get_binary(16#01)),
  ?_assertEqual(<<>>, parser:get_binary(<<>>))].

test_checksum() ->
  [?_assertEqual(225, parser:checksum(<<5, 5, 5, 5, 5, 5>>)),
  ?_assertEqual(255, parser:checksum(<<0>>)),
  ?_assertEqual(225, parser:checksum(<<256, 5, 5, 5, 5, 10>>))
  ].
