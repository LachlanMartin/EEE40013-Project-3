--
-- VHDL Test Bench Created from source file ioport.vhd -- 14:31:54 10/20/2004
--
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

USE ieee.std_logic_textio.ALL;
USE std.textio.ALL;
use work.debug.ALL;

ENTITY IOPortTestbench IS
END IOPortTestbench;

ARCHITECTURE behavior OF IOPortTestbench IS 

   constant clockHigh   : time := 50 ns; 
   constant clockLow    : time := 50 ns; 
   constant clockPeriod : time := clockHigh + clockLow; 

   SIGNAL reset   :  std_logic;
   SIGNAL clock   :  std_logic;
   SIGNAL writeEn :  std_logic;
   SIGNAL addr    :  std_logic_vector(2 downto 0);
   SIGNAL dataIn  :  std_logic_vector(15 downto 0);
   SIGNAL dataOut :  std_logic_vector(15 downto 0);
   SIGNAL portIO  :  std_logic_vector(15 downto 0);
   SIGNAL pinIn   :  std_logic_vector(15 downto 0);
   SIGNAL pinOut  :  std_logic_vector(15 downto 0);
   SIGNAL pinDrv  :  std_logic_vector(15 downto 0);

   -- Register addresses
   constant PDORAddress    : std_logic_vector(2 downto 0) := "000";
   constant PDDRAddress    : std_logic_vector(2 downto 0) := "001";
   constant PSORAddress    : std_logic_vector(2 downto 0) := "010";
   constant PCORAddress    : std_logic_vector(2 downto 0) := "011";
   constant PTORAddress    : std_logic_vector(2 downto 0) := "100";
   constant PDIRAddress    : std_logic_vector(2 downto 0) := "101";

   signal simComplete : boolean := false;
   
BEGIN

   uut:
   entity work.ioport 
   PORT MAP(
      reset   => reset,
      clock   => clock,
      writeEn => writeEn,
      addr    => addr,
      dataIn  => dataIn,
      dataOut => dataOut,
      pinIn   => pinIn,
      pinOut  => pinOut,
      pinDrv  => pinDrv
   );

   --****************************************************
   -- Clock Generator
   --
   ClockGen:
   process

   begin
      clock <= '0';
      wait until reset'event and reset = '0';

      while not simComplete loop
         clock <= '0';
         wait for clockHigh;
         clock <= '1';
         wait for clockLow;
      end loop;

      wait; -- stop process looping
   end process ClockGen;

   --****************************************************
   -- Implements the 3-state pin drive
   --
   pinMux:
   process (pinDrv, pinOut)
   begin
      -- portIO is INOUT
      portIO <= (others => 'Z');
      for bitNum in portIO'left downto portIO'right loop
         if (pinDrv(bitNum) = '1') then
            portIO(bitNum)  <= pinOut(bitNum);
         end if;
      end loop;      
   end process;
   
   pinIn <= portIO;
   
   --****************************************************
   -- Stimulus Generator
   --
   Stimulus:
   process

      --***************************************************
      -- Simulates a bus master writing data to mem[address]
      --
      --  newAddress => address to write to
      --  newData    => data to write
      --
      procedure busWrite ( newAddress : in std_logic_vector(2 downto 0);
                           newData    : in std_logic_vector(15 downto 0)
                         ) is
      begin

         -- Set up write
         wait until rising_edge(clock); -- replace with your code

         wait for 10 ns; -- space writes apart for visibility
      end;

      --***************************************************
      -- Simulates a bus master reading data from mem[address]
      --
      --  newAddress => address to read from
      --  newData    => data read
      --
      procedure busRead ( newAddress : in  std_logic_vector(2 downto 0);
                          newData    : out std_logic_vector(15 downto 0)
                        ) is
      begin
         -- Set up read - this is a combinational path
         wait until rising_edge(clock); -- replace with your code

         -- Capture read data on clock edge
         -- your code

         wait for 10 ns; -- space reads apart for visibility
      end;

      --***********************************************************
      -- Used to check an assertion of equality and prints a message
      -- to log file and transcript if discrepancy
      -- Asserts at error level when this occurs.
      --
      --  msg => message to print
      --  actualIO   => actual value on I/O pins
      --  expectedIO => expected value on I/O pins
      --
      procedure checkValue( msg        : in string; 
                            actualIO   : in std_logic_vector (15 downto 0);
                            expectedIO : in std_logic_vector (15 downto 0)
                            ) is

      variable assertMsgBuffer : String(1 to 4096); -- string for assert message
      variable writeMsgBuffer : line; -- buffer for write messages

      BEGIN
            if (actualIO /= expectedIO) then 
               dwrite("*****" & msg & " - Error, Expected vs actual ", expectedIO, actualIO);
            else
               dwrite(msg & " - OK value = ", actualIO);
         end if;
      end;

      variable tempData : std_logic_vector(15 downto 0);

   begin -- Stimulus

      openLog("Results.txt");
      
      dwrite("Simulation starting.");

      -- initially undefined or reset state
      dataIn    <= (others => 'X');
      addr      <= (others => 'X');
      writeEn   <= '0';
      portIO    <= (others => 'Z');
      
      reset <= '1';
      wait for 105 ns;
      reset <= '0';

      wait until rising_edge(clock);
      
      -- **********************************************************
      -- Test: Initial port value is 3-state
      portIO <= (others => 'Z'); -- 3-state stimulus initially
      dwrite( "Undriven port (expect 3-state)." );
      checkValue("IOValue", portIO, "ZZZZZZZZZZZZZZZZ");

      -- **********************************************************
      -- Test: Output 0x0000
      dwrite( string'("Configuring port as output 0x0000.") );
      busWrite( PDORAddress, x"0000" ); -- value to output
      busWrite( PDDRAddress, x"FFFF" ); -- set as all output
      checkValue("IOValue", portIO, x"0000");

      -- Your code for other tests
      
      
      wait for 4*clockPeriod;

      simComplete <= true;

      dwrite("Simulation completed.");
    
      closeLog;
         
      wait;

   end process Stimulus;

end architecture behavior;
