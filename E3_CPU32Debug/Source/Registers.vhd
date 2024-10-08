--============================================================================
-- Creates a 3-port (WRR) memory from two 2-port (WR) distributed RAM memories
--
-- This synthesizes to reasonably efficient hardware
-- Rather brute force using two memories!
--
-- Memories are implemented as distributed RAM
--
--============================================================================

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

entity Registers is
   Generic ( 
      dataWidth    : integer := 32;
      addressWidth : integer := 5
      );
   Port ( 
      reset    : in  std_logic;
      clock    : in  std_logic;
      -- Port A - write port
      writeA   : in  std_logic;
      addressA : in  std_logic_vector(addressWidth-1 downto 0);
      dataInA  : in  std_logic_vector(dataWidth-1    downto 0);
      -- Port B - read port
      addressB : in  std_logic_vector(addressWidth-1 downto 0);
      dataOutB : out std_logic_vector(dataWidth-1    downto 0);
      -- Port C - read port
      addressC : in  std_logic_vector(addressWidth-1 downto 0);
      dataOutC : out std_logic_vector(dataWidth-1    downto 0)
      );
end Registers;

architecture DistributedRAM of Registers is

type RegType is array (0 to (2**addressWidth)-1) of std_logic_vector(dataWidth-1 downto 0) ;

-- Two memory arrays are required to implement 3-port WRR memory
signal regBank1 : RegType:= (others => (others=>'0'));
signal regBank2 : RegType:= (others => (others=>'0'));

signal internalWriteA : boolean;

constant zeroAddress : std_logic_vector(addressWidth-1 downto 0) := (others => '0');

begin

   -- Prevent writes to R0 which is read only
   internalWriteA <= (writeA = '1') and (addressA /= zeroAddress);
  
   -- Implement one write port
   writePort:
   process (clock)
   begin
      if rising_edge(clock) then
         if internalWriteA then
            -- write to both memory arrays
            regBank1(to_integer(unsigned(addressA))) <= dataInA;
            regBank2(to_integer(unsigned(addressA))) <= dataInA;
         end if;
      end if;
   end process;

   -- Implement two read ports
   dataOutB <= regBank1(to_integer(unsigned(addressB)));
   dataOutC <= regBank2(to_integer(unsigned(addressC)));

end architecture DistributedRAM;