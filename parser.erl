-module(parser).
-compile(export_all).

-include("constants.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%goal: parse simple AT command frames
%{command, }, {option, }, {parameter, }, {device, self | longaddress of destination}
% self is the local connected xbee

% Components of a frame
% [?FRAME_DELIMITER, Count, Packet, Checksum]
build_frame(FrameSpec) ->
  Packet = build_packet(FrameSpec),
  Count = byte_size(Packet),
  CheckSum = checksum(Packet),
  {ok, iolist_to_binary([<<?FRAME_DELIMITER>>, <<Count:16>>, Packet, <<CheckSum:8>>])}.

%% Prints out the frame as a hex string. 
print_frame(FrameSpec) ->
  mochihex:to_hex(build_frame(FrameSpec)).

checksum(Packet) ->
  Result = lists:foldl(fun(X, Sum) -> X + Sum end, 0, binary:bin_to_list(Packet)),
 16#FF - (Result rem 256). 

build_packet(FrameSpec) ->
  iolist_to_binary(<<"hello">>).

%% Valid inputs: binary or hex (as a string) or as a straight hex/int
get_device(Command) ->
  case proplists:get_value(device, Command) of
    self ->
      {ok, self};
    undefined ->
      {ok, self};
    Device when is_list(Device) ->
      {ok, mochihex:to_bin(Device)};
    Device when is_binary(Device) ->
      {ok, Device};
    Device when is_integer(Device) ->
      {ok, <<Device:64>>};
    _ ->
      {error, device_bad_format}
  end.

%% Valid inputs: binary or ASCII
get_command(Command) ->
  case proplists:get_value(command, Command) of
    At when is_binary(At) ->
      {ok, At};
    At when is_list(At) ->
      {ok, list_to_binary(At)};
    undefined ->
      {error, command_undefined};
    _ ->
      {error, command_bad_format}
  end.
