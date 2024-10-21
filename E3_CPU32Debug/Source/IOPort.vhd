--=============================================================
--  Basic I/O port
--=============================================================
library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

entity IOPort is
    Port ( reset   : in  std_logic;
           clock   : in  std_logic;
           writeEn : in  std_logic;
           addr    : in  std_logic_vector(2 downto 0);
           dataIn  : in  std_logic_vector(15 downto 0);
           dataOut : out std_logic_vector(15 downto 0);
           pinIn   : in  std_logic_vector(15 downto 0);
           pinOut  : out std_logic_vector(15 downto 0);
           pinDrv  : out std_logic_vector(15 downto 0)
           );
end IOPort;

-- Port    ; Description
-- =========================================================================
-- reset   : Async. reset
-- clock   : Clock for all sync circuits.
-- writeEn : Write Enable
-- addr    : Address line (selects between various registers
-- dataIn  : Data in from CPU
-- dataOut : Data out to CPU
-- pinIn   : Port pin input lines
-- pinOut  : Port pin output lines
-- pinDrv  : Port pin 3-state enable (drive).
-- =========================================================================

-- Memory map  (word addresses)
--+===========================+
--|  PDOR                     |  000   R/W
--|                           |
--+===========================+
--|  PDDR                     |  001   R/W
--|  1=> output, 0=> input    |
--+===========================+
--|  PSOR (pseudo register)   |  010   W
--|  1=> set bit 0=> nil      |
--+===========================+
--|  PCOR (pseudo register)   |  011   W
--|  1=> clear bit 0=> nil    |
--+===========================+
--|  PTOR (pseudo register)   |  100   W
--|  1=> toggle bit 0=> nil   |
--+===========================+
--|  PDIR (pseudo register)   |  101   R
--|                           |
--+===========================+

--  Writes to the PDOR are registered and appear on pins that
--  are configured as outputs.  Reads from PDOR return its contents.
--
--  Writes to the PDDR are registered and control the pin pddr.
--  Reads from PDDR return its contents.
--  The PDDR controls the pddr of the I/O pins:
--     1=> pdor pins are an output and reflect the value contained
--         in the PDOR register
--     0=> pdor pins are inputs (output drivers are 3-state)
--
--  Reads from PDIR location return the pin values after resynching to clock.
--  
--  Writes to PSOR set bits in PDOR corresponding to 1's in the value written.
--
--  Writes to PCOR clear bits in PDOR corresponding to 1's in the value written.
--
--  Writes to PTOR toggle bits in PDOR corresponding to 1's in the value written.
--
architecture Behavioral of IOPort is

-- Registers
signal pdor : std_logic_vector(15 downto 0);
signal pddr : std_logic_vector(15 downto 0);
signal pdir : std_logic_vector(15 downto 0);

--================================================
-- Memory addresses within PORT I/O (as enum for readability)
type PortAddress is (
      A_pdor,
      A_pddr,
      A_psor,
      A_pcor,
      A_ptor,
      A_pdir,
      A_unused_6,
      A_unused_7,
      A_invalid
); 

signal portAddr : PortAddress;

--================================================
-- Convert slv to address enum
function to_PortAddress( signal addr : in std_logic_vector(2 downto 0))
         return PortAddress is
begin   
   if (is_X(addr)) then
      -- Just to catch invalid values in simulation
      return A_invalid;
   end if;
   return PortAddress'val(to_integer(unsigned(addr)));
end function;

begin

   -- Use this signal to 'select' registers in case statements
   -- This is overkill but will make deugging easier.
   portAddr <= to_PortAddress(addr);

inputSynch:
   -- Implements input synchronization to revent instability 
   -- when sampling asynchronous pins
   -- This should implement 2-level synchronization
   process (reset, clock) 
      variable sync : std_logic_vector(15 downto 0);
    begin
        if reset = '1' then
            pdir <= (others => '0'); -- Reset pdir on reset
            sync := (others => '0');
        elsif rising_edge(clock) then
            pdir <= sync; -- Sample pinIn on clock edge
            sync := pinIn;
        end if;
    end process;
   
readControl:
   -- Implements the read function on the internal bus
   process (portAddr, pdor, pddr, pdir)
    begin
        case portAddr is
            when A_pdor =>
                dataOut <= pdor; -- Read from PDOR
            when A_pddr =>
               dataOut <= pddr;
            when A_pdir =>
                dataOut <= pdir; -- Read from PDIR
            when others =>
                dataOut <= (others => 'X'); -- Default case
        end case;
    end process readControl;

writeControl:
   -- Implements the write function on the internal bus
   process (reset, clock)
    begin
        if reset = '1' then
            pdor <= (others => '0'); -- Reset PDOR on reset
            pddr <= (others => '0'); -- Reset PDDR on reset
        elsif rising_edge(clock) then
            if writeEn = '1' then
                case portAddr is
                    when A_pdor =>
                        pdor <= dataIn; -- Write to PDOR
                    when A_pddr =>
                        pddr <= dataIn; -- Write to PDDR
                    when A_psor =>
                        pdor <= pdor or dataIn; -- Set bits in PDOR
                    when A_pcor =>
                        pdor <= pdor and not dataIn; -- Clear bits in PDOR
                        -- pdor <= (pdor xor dataIn) and not dataIn
                    when A_ptor =>
                        pdor <= pdor xor dataIn; -- Toggle bits in PDOR
                    when others =>
                        null; -- No action for other addresses
                end case;
            end if;
        end if;
    end process writeControl;

   -- General connections
   pinOut <= pdor;
   pinDrv <= pddr;
   
end Behavioral;