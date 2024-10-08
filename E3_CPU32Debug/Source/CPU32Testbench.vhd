--******************************************************************
--  Testbench for CPU32
--******************************************************************

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

use work.cpupackage.ALL;
use work.debug.ALL;

entity cpuTestbench is
end cpuTestbench;

architecture behavior OF cpuTestbench is 

-- Signals for UUT ports
signal reset      : std_logic;
signal clock      : std_logic;
signal pcOut      : std_logic_vector(31 downto 0);

signal pinIn      : std_logic_vector(15 downto 0);
signal pinOut     : std_logic_vector(15 downto 0);
signal pinDrv     : std_logic_vector(15 downto 0);

signal portIO     : std_logic_vector(15 downto 0);

constant clockHigh   : time := 50 ns; 
constant clockLow    : time := 50 ns; 
constant clockPeriod : time := clockHigh + clockLow; 

signal complete : boolean;

begin

   --****************************************************
   -- Clock Generator
   --
   ClockGen:
   process

   begin
      while not complete loop
         clock <= '0';  wait for clockHigh;
         clock <= '1';  wait for clockLow;
      end loop;

      wait; -- stop process looping
   end process ClockGen;

   --****************************************************
   -- The CPU
   --
   theCPU:
   entity work.cpu32(Behavioral) 
   Port Map (
      reset       => reset,
      clock       => clock,
      pcOut       => pcOut,
      
      pinIn       => pinIn,
      pinOut      => pinOut,
      pinDrv      => pinDrv
      );

   --****************************************************
   -- Implements the IOport 3-state logic
   --
   process (pinDrv, pinOut)
   begin
      -- Real portIO are INOUT
      portIO <= (others => 'Z');
      for bitNum in portIO'left downto portIO'right loop
         if (pinDrv(bitNum) = '1') then
            portIO(bitNum)  <= pinOut(bitNum);
         end if;
      end loop;		
   end process;

   pinIn <= portIO;
	
   --****************************************************
   -- Testbench
   -- Just provides a clock and terminates simulation
   --
   testbench:
   process

   begin

      openLog( "results.txt" );
      
      reset <= '1';
      wait for 10 ns;
      wait until rising_edge(clock);
      reset <= '0';

      -- For initial testing - output only e.g. light-chaser example
      portIO <= "ZZZZZZZZZZZZZZZZ";
      --portIO <= "00000001ZZZZZZZZ";

      wait for 40000 ns; -- Adjust to suit code execution time
      dwrite( string'("Simulation completed.") );

      -- end simulation
      complete <= true;
      
      closeLog;
      wait;

   end process testbench;

end architecture behavior;
