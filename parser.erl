-module(parser).
-compile(export_all).

-include("constants.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%goal: build simple AT command frames
%{command, }, {option, }, {parameter, }, {device, me | longaddress of destination}
% me is the local connected xbee

% Components of a frame
% [?FRAME_DELIMITER, Count, Packet, Checksum]
build_frame(FrameSpec) ->
  {Message, Result} = build_packet(FrameSpec#frame.type, FrameSpec),
  case Message of 
    ok ->
      Count = byte_size(Result),
      CheckSum = checksum(Result),
      {ok, iolist_to_binary([<<?FRAME_DELIMITER>>, <<Count:16>>, Result, <<CheckSum:8>>])};
    error ->
      {error, Result} 
  end.

%% Prints out the frame as a hex string. 
print_frame(FrameSpec) ->
  {Msg, Frame} = build_frame(FrameSpec),
  case Msg of 
    ok ->
      mochihex:to_hex(Frame);
    _ ->
      {Msg, Frame}
  end.

checksum(Packet) ->
  Result = lists:foldl(fun(X, Sum) -> X + Sum end, 0, binary:bin_to_list(Packet)),
 16#FF - (Result rem 256). 

build_packet(FrameType, FrameSpec) ->
  case FrameType of 
    ?AT_FRAME ->
    % ?AT_FRAME -> [FrameType, FrameId, Command, Value]
      {ok, iolist_to_binary([<<?AT_FRAME>>, 
                  get_binary(FrameSpec#frame.frame_id), 
                  get_binary(FrameSpec#frame.at_command),
                  get_binary(FrameSpec#frame.value)])};
    ?AT_REMOTE_FRAME ->
      % ?AT_REMOTE_FRAME -> [FrameType, FrameId, Device, DestAddr, ApplyChanges, Command, Value]
      {error, unimplemented};
    _ ->
      {error, unsupported_frame_type}
  end.

%% Valid inputs: binary or hex (as a string) or as a straight hex/int
get_device(FrameSpec) ->
  case FrameSpec#frame.device of
    me ->
      {ok, me};
    undefined ->
      {ok, me};
    Device when is_list(Device) ->
      {ok, mochihex:to_bin(Device)};
    Device when is_binary(Device) ->
      {ok, Device};
    Device when is_integer(Device) ->
      {ok, <<Device:64>>};
    _ ->
      {error, device_bad_format}
  end.

%% Get binary value of a parameter
get_binary(Value) ->
  case Value of
    Value when is_binary(Value) ->
      Value;
    Value when is_list(Value) ->
      list_to_binary(Value);
    Value when is_integer(Value) ->
      mochihex:to_bin(mochihex:to_hex(Value));
    _ ->
      {error, bad_format}
  end.
