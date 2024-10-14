--============================================================================
-- This files contains some general purpose debugging code
--============================================================================

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

USE ieee.std_logic_textio.ALL;
USE std.textio.ALL;

package debug is

   procedure dwrite ( message : in string );
   procedure dwrite ( message : in string; slv : in std_logic_vector );
   procedure dwrite ( message : in string; slv1: in std_logic_vector; slv2 : in std_logic_vector);
   
   procedure openLog( filePath : in string );
   procedure closeLog;
   
   procedure setLogging( mode : in boolean );

end debug;

package body debug is

   --  File to log results to
   FILE logFile: TEXT;

   -- Set the following to true to turn on messages
   shared variable logging : boolean := false;

   --=================================================
   -- Open given log file
   -- Logging is turned on
   --=================================================
   procedure openLog( filePath : in string 
                      ) is
   variable openStatus : file_open_status;

   begin
      file_open(openStatus, logFile, filePath, WRITE_MODE);
      assert openStatus=OPEN_OK
         report "Log file open failure."
         severity error;
      logging := true; 
   end procedure;
               
   --=================================================
   -- Close log file
   -- Logging is turned off
   --=================================================
   procedure closeLog is

   begin
      file_close(logFile);    
      logging := false; 
   end procedure;

   --=================================================
   -- Logging is turned on/off
   --=================================================
   procedure setLogging( mode : in boolean ) is
   begin
      logging := mode;
   end procedure;

   --*******************************************************
   -- Write a message to the logfile & transcript
   --
   --  message => string to write
   --
   procedure dwrite ( message : string
                      ) is
                      
   variable assertMsgBuffer : String(1 to 4096); -- string for assert message
   variable writeMsgBuffer  : line; -- buffer for write messages

   begin
      if (not logging) then
         write( writeMsgBuffer, message );
         assertMsgBuffer(writeMsgBuffer.all'range) := writeMsgBuffer.all;
         writeline(logFile, writeMsgBuffer);
         deallocate(writeMsgBuffer);
         assert (false) report assertMsgBuffer severity note;
      end if;
   end;

   --**************************************************************
   -- Write a message and a slv in hex to the logfile & transcript
   --
   --  message => string to write
   --  slv     => std_logic_vector to write in hex
   --
   procedure dwrite ( message : string;
                      slv     : std_logic_vector
                    ) is
                    
   variable assertMsgBuffer : String(1 to 4096); -- string for assert message
   variable writeMsgBuffer  : line; -- buffer for write messages

   begin
      if (not logging) then
         write( writeMsgBuffer, message );
--         hwrite( writeMsgBuffer, slv );
         write( writeMsgBuffer, to_bitvector(slv), left );
         assertMsgBuffer(writeMsgBuffer.all'range) := writeMsgBuffer.all;
         writeline(logFile, writeMsgBuffer);
         deallocate(writeMsgBuffer);
         assert (false) report assertMsgBuffer severity note;
      end if;
   end;

   --**************************************************************
   -- Write a message and a two slvs in hex to the logfile & transcript
   --
   --  message   => string to write
   --  slv1,slv2 => std_logic_vectors to write in hex
   --
   procedure dwrite ( message : string;
                      slv1    : std_logic_vector;
                      slv2    : std_logic_vector
                    ) is
                    
   variable assertMsgBuffer : String(1 to 4096); -- string for assert message
   variable writeMsgBuffer : line; -- buffer for write messages

   begin
      if (not logging) then
         write( writeMsgBuffer, message );
--         hwrite( writeMsgBuffer, slv1 );
         write( writeMsgBuffer, to_bitvector(slv1), left );
         write( writeMsgBuffer, string'(" : ") );
--         hwrite( writeMsgBuffer, slv2 );
         write( writeMsgBuffer, to_bitvector(slv2), left );
         assertMsgBuffer(writeMsgBuffer.all'range) := writeMsgBuffer.all;
         writeline(logFile, writeMsgBuffer);
         deallocate(writeMsgBuffer);
         assert (false) report assertMsgBuffer severity note;
      end if;
   end;
    
end debug;