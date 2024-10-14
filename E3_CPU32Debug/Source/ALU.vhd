--******************************************************************
--  ALU.vhd
--******************************************************************

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

use work.cpupackage.ALL;
-- synopsys translate_off
use work.debug.ALL;
-- synopsys translate_on

entity alu is
    Port ( aluOp     : in  std_logic_vector(2 downto 0);
           operand1  : in  unsigned(31 downto 0);
           operand2  : in  unsigned(31 downto 0);
           aluOutput : out unsigned(31 downto 0);
           Z,N,V,C   : out std_logic;
           clock, reset : in std_logic;
           doFlags : in std_logic;
           aluStart : in std_logic;
           aluComplete : out std_logic
         );
end alu;

architecture Behavioral of alu is

constant zero       : unsigned( 3 downto 0) := "0000";
signal   aluOutputx : unsigned(32 downto 0);
    
-- Temporary flag signals
signal tempZ, tempN, tempV, tempC : std_logic;
signal multResult : unsigned(31 downto 0) := (others => '0');
signal multStart : std_logic := '0';
signal multComplete : std_logic;

begin

    MultiplierInstance: entity work.Multiplier5Cycle
        port map (
            Clock => clock,
            Reset => reset,
            A => operand1(15 downto 0),
            B => operand2(15 downto 0),
            Q => multResult,
            Start => multStart,
            Complete => multComplete
        );

ALUProcess:
process (aluOp, operand1, operand2, aluOutputx, multResult, aluStart, multComplete)
begin

    tempV <= '0';
    aluComplete <= '1';
    multStart <= '0';
	
    case aluOp is
        when ALUopAdd =>
            aluOutputx <= ('0' & operand1) + ('0' & operand2);
            tempV <= (not aluOutputx(31) and (operand1(31) and operand2(31))) or
                     (aluOutputx(31) and (not operand1(31) and not operand2(31)));
        when ALUopSub =>
            aluOutputx <= ('0' & operand1) - unsigned('0' & operand2);
            tempV <= (not aluOutputx(31) and (operand1(31) and not operand2(31))) or
                     (aluOutputx(31) and (not operand1(31) and operand2(31)));
        when ALUopAnd =>
            aluOutputx <= '0' & (operand1 and operand2);
        when ALUopOr =>
            aluOutputx <= '0' & (operand1 or operand2);
        when ALUopEor =>
            aluOutputx <= '0' & (operand1 xor operand2);
        when ALUopRor =>
            -- Rotate through carry
            aluOutputx <= operand1(operand1'right) & '0' & (operand1(operand1'left downto 1));
        when ALUopSwap =>
            aluOutputx <= '0' & operand2(15 downto 0) & operand2(31 downto 16);
        when ALUopMult =>
            aluOutputx <= '0' & multResult;
            multStart <= aluStart;
            aluComplete <= multComplete;
        when others =>
            aluOutputx <= (others => 'X');
    end case;
 
    tempN <= aluOutputx(31);
    tempC <= aluOutputx(32);
    if (aluOutputx(31 downto 0) = x"00000000") then
        tempZ <= '1';
    else
        tempZ <= '0';
    end if;

    aluOutput <= aluOutputx(31 downto 0);

end process ALUProcess;

-- New clocked process to update flags
FlagUpdateProcess:
process (clock, reset, doFlags)
begin
    if reset = '1' then
        Z <= '0';
        N <= '0';
        V <= '0';
        C <= '0';
    elsif rising_edge(clock) and doFlags = '1' then
        Z <= tempZ;
        N <= tempN;
        V <= tempV;
        C <= tempC;
    end if;
end process FlagUpdateProcess;

end Behavioral;