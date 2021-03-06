%% Bytes to escape
-define(FRAME_DELIMITER, 16#7E).
-define(ESCAPE, 16#7D).
-define(XON, 16#11).
-define(XOFF, 16#13).
-define(ESCAPE_CHAR, 16#20).

%% API frame names and values
-define(AT_COMMAND_FRAME, 16#08).
-define(AT_COMMAND_QUEUE_PARAM_VALUE_FRAME, 16#09). % queues but doesn't apply value
-define(ZB_TRANSMIT_REQUEST_FRAME, 16#10).
-define(ZB_EXPLICIT_ADDRESSING_COMMAND_FRAME, 16#11).
-define(AT_REMOTE_COMMAND_FRAME, 16#17).
-define(CREATE_SOURCE_RT_FRAME, 16#21).
-define(AT_COMMAND_RESPONSE_FRAME, 16#88).
-define(MODEM_STATUS_FRAME, 16#8A).
-define(ZB_TRANSMIT_STATUS_FRAME, 16#8B).
-define(ZB_RECEIVE_PACKET_FRAME, 16#90).
-define(ZB_EXPL_RX_INDICATOR_FRAME, 16#91).
-define(ZB_IO_DATA_SAMPLE_RX_INDICATOR_FRAME, 16#92).
-define(XB_SENSOR_READ_FRAME, 16#94).
-define(NODE_ID_FRAME, 16#95).
-define(AT_REMOTE_COMMAND_RESPONSE_FRAME, 16#97).
-define(OTA_FIRMWARE_UPDATE_STATUS_FRAME, 16#A0).
-define(ROUTE_RECORD_INDICATOR_FRAME, 16#A1).
-define(MANY_TO_ONE_ROUTE_REQUEST_INDICATOR_FRAME, 16#A3).

% Record types
-record(frame, {type=?AT_COMMAND_FRAME,
                frame_id=16#1,
                device=me,
                at_command=[],
                value=[]}).
