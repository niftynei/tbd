-module(decomposer).
-include_lib("eunit/include/eunit.hrl").

-include("constants.hrl").
-compile(export_all).

%% todo: parse status codes to an atom

frame([?FRAME_DELIMITER, S1, Size | Frame]) -> 
   [CheckSum | Packet ] = lists:reverse(Frame),
   [FrameType | Data] = lists:reverse(Packet), 
   true = verify_size(Size + S1, Packet),
   true = verify_checksum(CheckSum, Packet),
   extract_frame(FrameType, Data);
frame(Bin) when is_binary(Bin) ->
  frame(binary_to_list(Bin));
frame(List) when is_list(List) ->
  frame(mochihex:to_bin(List));
frame(Int) when is_integer(Int) ->
  frame(mochihex:to_hex(Int));
frame(_) ->
  error(not_a_frame).

extract_frame(FrameType, Data) ->
  case FrameType of
    ?AT_COMMAND_FRAME->
        [FrameId, Cmd1, Cmd2 | Value] = Data,
          {ok, #frame{type=FrameType,
                      at_command=[Cmd1,Cmd2],
                      value=Value,
                      frame_id=FrameId}};
    ?AT_COMMAND_RESPONSE_FRAME ->
        [FrameId, Cmd1, Cmd2, Status | Value] = Data,
          {ok, #frame{type=FrameType,
                      at_command=[Cmd1,Cmd2],
                      value=Value,
                      frame_id=FrameId}, 
               {options, [{status, Status}]}};
    ?AT_REMOTE_COMMAND_FRAME ->
        [FrameId, Add1, Add2, Add3, Add4, Add5, Add6, Add7, Add8, Dest1, Dest2, CmdOpts, Cmd1, Cmd2 | Value] = Data,
          Address = list_to_binary([Add1,Add2,Add3, Add4,Add5,Add6,Add7,Add8]),
          {ok, #frame{type=FrameType,
                      at_command=[Cmd1,Cmd2],
                      device=Address,
                      value=Value,
                      frame_id=FrameId}, 
               {options, [{destination_address, [Dest1,Dest2]}, 
                          {command_options, CmdOpts}]}};
    ?AT_REMOTE_COMMAND_RESPONSE_FRAME ->
        [FrameId, Add1, Add2, Add3, Add4, Add5, Add6, Add7, Add8, Dest1, Dest2, Cmd1, Cmd2, Status | Value] = Data,
          Address = list_to_binary([Add1,Add2,Add3,Add4,Add5,Add6,Add7,Add8]),
          {ok, #frame{type=FrameType,
                      at_command=[Cmd1,Cmd2],
                      device=Address,
                      value=Value,
                      frame_id=FrameId}, 
               {options, [{destination_address, [Dest1,Dest2]}, 
                          {status, Status}]}};
    _ ->
      error(frame_type_not_supported)
  end.

verify_size(Size, Packet) ->
  case Size =:= lists:foldl(fun(_, Sum) -> 1 + Sum end, 0, Packet) of 
    true -> true;
    false -> error(data_size_error)
  end.

verify_checksum(CheckSum, Packet) ->
  case 16#FF =:= CheckSum + lists:foldl(fun(X, Sum) -> X + Sum end, 0, Packet) rem 256 of
    true -> true;
    false -> error(data_checksum_failed)
  end.
