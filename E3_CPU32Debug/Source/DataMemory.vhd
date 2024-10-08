--============================================================================
-- Creates a Read/Write memory from block RAM
--
--============================================================================


library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

ENTITY DataMemory IS
   Generic ( 
      dataWidth    : integer := 32;
      addressWidth : integer := 10
      );
	port (
      clock : IN  std_logic;
      we    : IN  std_logic;
      addr  : IN  std_logic_vector(addressWidth-1 downto 0);
      din   : IN  std_logic_vector(dataWidth-1    downto 0);
      dout  : OUT std_logic_vector(dataWidth-1    downto 0)
   );
END DataMemory;

ARCHITECTURE RTL OF DataMemory IS

type MemType is array (0 to (2**addressWidth)-1) of std_logic_vector(dataWidth-1 downto 0);

signal mem : MemType := (others => (others =>'0'));

begin
    
   process (clock)
   begin 
      if rising_edge(clock) then
         if (we = '1') then
            -- Implement sync write port
            mem(to_integer(unsigned(addr))) <= din;
            -- Implement sync read port with READ after WRITE for block RAM
            dout                    <= din;
         else
            -- Implement sync read port
            dout <= mem(to_integer(unsigned(addr)));
         end if;
      end if;   
   end process;
   
end architecture RTL;
