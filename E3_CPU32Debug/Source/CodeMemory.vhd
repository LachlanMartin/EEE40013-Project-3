--============================== ==============================================
-- Creates a Read/Write memory from block RAM
--
-- Memory is initialised from 'objectFileName'
--
--============================================================================

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

USE ieee.std_logic_textio.ALL;
USE std.textio.ALL;

ENTITY CodeMemory IS
   Generic ( 
      dataWidth      : integer := 32;
      addressWidth   : integer := 10;
      objectFileName : string  := "codememory.mif" -- path to ROM contents file
      );
	port (
      clock : IN  std_logic;
      en    : IN  std_logic;
      addr  : IN  std_logic_vector(addressWidth-1 downto 0);
      dout  : OUT std_logic_vector(dataWidth-1    downto 0)
      );
END CodeMemory;

ARCHITECTURE RTL OF CodeMemory IS

type ROMType is array (0 to (2**addressWidth)-1) of std_logic_vector(dataWidth-1 downto 0);

--=====================================================================
--  This function reads the ROM contents from a file.
--  Its return value should be assigned to the ROM.
--=====================================================================
impure function loadROM (fileName : in string) return ROMType is                                                   

   FILE     memoryFile        : TEXT; --  File to read initial memory contents from
   
   variable openStatus        : file_open_status;
   variable value             : std_logic_vector(dataWidth-1 downto 0);
   variable lineOK            : boolean := true;
   variable aLine             : line;
   variable address           : natural;
   variable assertMsgBuffer   : String(1 to 4096); -- string for assert message
   
   -- Memory image (defaults to zero contents)
   variable mem               : ROMType := (others => (others =>'0'));
   variable filepath          : line;
   begin
      -- Open file
--    Use this if you want to store the source file in another directory
--      write(filepath, string'("/home/peter/Documents/Work/EEE40013_20_S2/Laboratory/E2_CPU32Debug/Simulate/"));
      write(filepath, fileName);
      file_open(openStatus, memoryFile, filepath.all, READ_MODE);
      if openStatus/=OPEN_OK then
         report "Memory file open failure """&fileName&""""
         severity error;
      else
         report "Memory file opened OK - """&fileName&""""
         severity note;
      end if;

      -- Load ROM
      for address in ROMType'range loop
         exit when endfile(memoryFile); -- exit on short file
         readline(memoryFile, aLine);
         read(aLine, value, lineOK); -- try for a number

         if (not lineOK) then
            --assertMsgBuffer(aLine.all'range) := aLine.all;
            report "Memory File contents invalid: Line#"&natural'image(address)
            severity error;
         end if;
         mem(address) := value;
      end loop;
      
      file_close(memoryFile);
      return mem;
      
   end function;

   -- The actual ROM - Note this is a constant even though it is initialised
   -- by the above function.
   constant mem : ROMType := loadROM( objectFileName );

begin 

   process (clock)
      begin 
      -- Implement sync read port
      -- dout is unchanged if not enabled (Used as IR)
      if (rising_edge(clock)) then
         if (en = '1') then
            dout <= mem(to_integer(unsigned(addr)));
         end if;
      end if;   
   end process;
   
end architecture RTL;