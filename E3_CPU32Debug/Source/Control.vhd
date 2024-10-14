--------------------------------------------------------------------------------
-- Control state machine
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

use work.cpupackage.ALL;
-- synopsys translate_off
use work.debug.ALL;
-- synopsys translate_on

entity Control is
    Port ( reset        : in  std_logic;
           clock        : in  std_logic;

           ir           : in  std_logic_vector(31 downto 0);
           Z,N,V,C      : in  std_logic;

           regAWrite    : out std_logic;
           RegASource   : out RegASourceT; 

           doFlags      : out std_logic;

           loadPC       : out std_logic;
           loadIR       : out std_logic;
           writeEn      : out std_logic;
           PCSource     : out PCSourceT;

           aluStart : out std_logic;
           aluComplete : in std_logic
           );
end entity Control;

architecture Behavioral of Control is

type   CpuStateType is (fetch, decode, execute, dataRead, dataWrite); 

signal cpuState             : CpuStateType;
signal nextCpuState         : CpuStateType;

begin
    
   --************************************************************************
   -- Process to monitor the action in the system for debugging.
   --

   -- synopsys translate_off
   --monitor:
   --process( reset, clock )
   --
   --begin
   --
   --   if (clock'event and clock = '1') then
   --      case cpuState is
   --         when init =>
   --            dwrite("Initialisation" );
   --         when fetch => -- load next word into instruction register
   --            dwrite("Fetched data ", pc, codeMemDataOut);
   --         when decode =>
   --            dwrite("Decoding");
   --         when execute =>
   --            dwrite("Executing");
   --         when dataWrite =>     -- write general register to memory
   --            dwrite("Data write");
   --         when dataRead =>      -- read general register from memory
   --            dwrite("Data read");
   --         when others =>
   --            null;
   --      end case;
   --   end if;
   --end process monitor;
   -- synopsys translate_on
   --
   --
   --************************************************************************
   -- Synchronous elements
   --
   -- State vector
   --
   synchronous:
   process ( reset, clock )

   begin  
      if (reset = '1') then
         cpuState  <= fetch;
      elsif rising_edge(clock) then
         cpuState  <= nextCpuState;
      end if;
   end process synchronous;

   --************************************************************************
   -- Asynchronous elements
   --
   -- Next state function
   -- Control signals
   --
   combinational:
   process ( cpuState, ir, Z,N,V,C, aluComplete )

   --************************************************************************
   -- Determines if a conditional branch is taken
   --
   function doBranch( signal Z,N,V,C   : in std_logic;
                      signal ir        : in std_logic_vector(31 downto 0)
                     ) return boolean is

   variable res       : std_logic;
   variable condition : std_logic_vector(3 downto 0);

   begin

      condition := ir_branchCondition(ir);

      case condition(3 downto 1) is
         when BRA    =>  res :=  '1';              -- BRA / BSR
         when BCS    =>  res :=  C;                -- BCS=BLO / BCC=BHS (unsigned)
         when BEQ    =>  res :=  Z;                -- BEQ / BNE (both)
         when BVS    =>  res :=  V;                -- BVS / BVC (signed)
         when BMI    =>  res :=  N;                -- BMI / BPL (signed)
         when BLT    =>  res := (N xor V);         -- BLT / BGE (signed)
         when BLE    =>  res := (Z or (N xor V));  -- BLE / BGT (signed)
         when BLS    =>  res := (C or Z);          -- BLS / BHI (unsigned)
         when others =>  res := 'X';
      end case;

      return (condition(0) xor res) = '1';

   end function doBranch;

   -- Needed for use in case statements
   variable irOp : std_logic_vector(2 downto 0);
   variable irAluOp : std_logic_vector(2 downto 0);

   begin
      -- Default control signal values inactive
      -- Default selection for muxes
      loadIR        <= '0';

      loadPC        <= '0';
      pcSource      <= nextPC;

      regAWrite     <= '0';
      RegASource    <= aluOut;

      writeEn  <= '0';

      nextCpuState  <= fetch;

      irOp := ir_op(ir); -- extract opcode field
      irAluOp := ir_aluOp(ir); -- extract aluOpCode field
      doFlags <= '0';

      aluStart <= '0';

      case cpuState is

         when fetch => -- load next word into instruction register
            nextCpuState   <= decode;
            loadPC         <= '1';
            loadIR         <= '1';

         when decode =>
            case irOp is
               when "000" =>                                -- Ra <- Rb op Rc
                  nextCpuState <= execute;
                  aluStart <= '1';
               when "001" =>                                -- Ra <- Rb op sex(immed)
                  nextCpuState <= execute;
                  aluStart <= '1';
               when "010" =>                                -- Ra <- mem(Rb + sex(immed))
                                                            -- PC <- Rb op sex(immed)
                  nextCpuState <= execute;
               when "011" =>                                -- mem(Rb + sex(immed)) <- Rc
                  nextCpuState <= execute;
               when "100" =>
                  nextCpuState <= fetch;
                  if (ir_branchCondition(ir) = "0000") then     -- PC <- PC + offset
                     loadPC       <= '1';
                     PCSource     <= branchPC;
                  elsif (ir_branchCondition(ir) = "0001") then  -- PC <- PC + offset; Reg31 <- PC;
                     loadPC       <= '1';
                     PCSource     <= branchPC;
                     RegASource   <= reg31;
                     regAWrite    <= '1';
                  else                                      -- if (cond) PC <- PC + offset
                     if (doBranch( Z, N, V, C, ir)) then
                        loadPC    <= '1';
                        PCSource  <= branchPC;
                     else
                        PCSource  <= nextPC;
                     end if;
                  end if;
               when others =>
                  null;
            end case;

         when execute =>
            case irOp is
               when "000" | "001" =>  -- Ra <- Rb op Rc, Ra <- Rb op sex(immed)
                  if aluComplete = '1' then
                     regAWrite    <= '1';            
                     nextCpuState <= fetch;
                     if irAluOp /= "101" and irAluOp /= "110" then -- swap and ROR should not update the ALU Flags
                        doFlags <= '1';
                     end if;
                  else
                     nextCpuState <= execute;
                  end if;
               when "010" =>              
                  if (ir_regA(ir) /= "00000") then  -- Ra <- mem(Rb + sex(immed))
                     nextCpuState <= dataRead;
                  else                              -- PC <- Rb op sex(immed)
                     PCSource     <= jumpPC;
                     loadPC       <= '1';
                     nextCpuState <= fetch;
                  end if;
               when "011" =>                        -- mem(Rb + sex(immed)) <- Ra
                     nextCpuState <= dataWrite;
               when others =>
                  null;
            end case;

         when dataWrite =>     -- mem(Rb + sex(immed)) <- Rc
            writeEn  <= '1';
            nextCpuState  <= fetch;

         when dataRead =>      -- Ra <- mem(Rb + sex(immed))
            RegASource    <= dataMemOut;
            regAWrite     <= '1';       
            nextCpuState  <= fetch;

--         when others =>
            null;
      end case;
   end process combinational;

end architecture Behavioral;
