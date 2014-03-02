-module(composer).
-compile(export_all).

-include("constants.hrl").
-ifdef(TEST).
-compile(export_all).
-else.
-export([build_frame/1, print_frame/1]).
-endif.

%goal: build simple AT command frames
% me is the local connected xbee
%% todo: sanitize results with escape chars

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

%% Valid device address is 8 bytes
validate_device(Device) ->
    case byte_size(Device) =:= 8 of
      true -> true;
      false -> error(invalid_device_addr)
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
