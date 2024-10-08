--===================================================================
--  Toplevel wrapper for CPU32
--  Modify as required to make pin connections

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;


entity TopLevel is
    Port ( reset        : in     std_logic;
           clock50MHz   : in     std_logic;
           LEDs         : inout  std_logic_vector (7 downto 0);
           switches     : in     std_logic_vector (3 downto 0)
           );
end TopLevel;

architecture Behavioral of TopLevel is
      
signal pcOut   : std_logic_vector(31 downto 0);
signal pinIn   : std_logic_vector(15 downto 0);
signal pinOut  : std_logic_vector(15 downto 0);
signal pinDrv  : std_logic_vector(15 downto 0);
		  
begin


   --****************************************************
   -- The CPU
   --
   theCPU:
   entity work.cpu32
   Port Map (
      reset     => reset,
      clock     => clock50MHz,
      pcOut     => pcOut,
      pinIn     => pinIn,
      pinOut    => pinOut,
      pinDrv    => pinDrv
      );

   -- Input map directly
   pinIn <= "0000"&switches&LEDs;

--   process (LEDs, pinDrv, pinOut)
--   begin
--      -- LEDs are INOUT
--      LEDs <= (others => 'Z');
--      for bitNum in LEDs'left downto LEDs'right loop
--         if (pinDrv(bitNum) = '1') then
--            LEDs(bitNum)  <= pinOut(bitNum);
--         end if;
--      end loop;		
--   end process;

   -- Connect the PC until the IOPort is complete otherwise the design will optimise away!
	LEDs <= pcOut(LEDs'left downto LEDs'right);
   
end Behavioral;
