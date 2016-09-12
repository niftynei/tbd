-module(composer).
-include_lib("eunit/include/eunit.hrl").

-include("constants.hrl").
-ifdef(EUNIT).
-compile(export_all).
-else.
-export([build_frame/1, print_frame/1, escape_packet/1]).
-endif.

%goal: build simple AT command frames
% me is the local connected xbee
%% todo: add options to datastruct for complex commands

% Components of a frame
% [?FRAME_DELIMITER, Count, Packet, Checksum]
build_frame(FrameSpec) ->
  {Message, Result} = case FrameSpec#frame.device of 
    me -> 
      build_packet(FrameSpec#frame.type, FrameSpec);
    _ when FrameSpec#frame.type =:= ?AT_COMMAND_FRAME -> 
      true = validate_device(get_binary(FrameSpec#frame.device)),
      build_packet(?AT_REMOTE_COMMAND_FRAME, FrameSpec);
    _ ->
      true = validate_device(get_binary(FrameSpec#frame.device)),
      build_packet(FrameSpec#frame.type, FrameSpec)
  end,
  case Message of 
    ok ->
      Count = byte_size(Result),
      CheckSum = checksum(Result),
      {ok, iolist_to_binary([<<?FRAME_DELIMITER>>, <<Count:16>>, Result, <<CheckSum:8>>])};
    error ->
      {error, Result} 
  end.

%% Escapes control flow characters that occur
escape_packet(<<?FRAME_DELIMITER, Packet/binary>>) ->
  escape(Packet,[?FRAME_DELIMITER]);
escape_packet(Packet) ->
  error(not_a_packet).
escape(<<>>, Acc) ->
  iolist_to_binary(lists:reverse(Acc));
escape(<<Char:8, Rest/binary>>, Acc) ->
  case lists:member(Char, [?FRAME_DELIMITER, ?ESCAPE, ?XON, ?XOFF]) of 
    true ->
      escape(Rest, [(Char bxor ?ESCAPE_CHAR), ?ESCAPE | Acc]);
    false -> 
      escape(Rest, [Char | Acc])
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
    ?AT_COMMAND_FRAME ->
    % ?AT_COMMAND_FRAME -> [FrameType, FrameId, Command, Value]
      {ok, iolist_to_binary([<<?AT_COMMAND_FRAME>>, 
                  get_binary(FrameSpec#frame.frame_id), 
                  get_binary(FrameSpec#frame.at_command),
                  get_binary(FrameSpec#frame.value)])};
    ?AT_REMOTE_COMMAND_FRAME ->
      % ?AT_REMOTE_COMMAND_FRAME -> [FrameType, FrameId, Device, DestAddr, ApplyChanges, Command, Value]
      {ok, iolist_to_binary([<<?AT_REMOTE_COMMAND_FRAME>>,
                   get_binary(FrameSpec#frame.frame_id),
                   get_binary(FrameSpec#frame.device),
                   get_binary(16#FFFE),
                   get_binary(16#02), % apply changes
                   get_binary(FrameSpec#frame.at_command),
                   get_binary(FrameSpec#frame.value)])};
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
