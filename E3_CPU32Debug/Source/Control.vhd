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

           loadPC       : out std_logic;
           loadIR       : out std_logic;
           writeEn      : out std_logic;
           PCSource     : out PCSourceT
           );
end entity Control;

architecture Behavioral of Control is

type   CpuStateType is (fetch, decode, execute, dataRead, dataWrite); 

signal cpuState             : CpuStateType;
signal nextCpuState         : CpuStateType;

signal branchOp : BraOp;

begin
    
   branchOp <= ir_braOp(ir);
    
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
   process ( cpuState, ir, Z,N,V,C )

   --************************************************************************
   -- Determines if a conditional branch is taken
   --
   function doBranch( signal Z,N,V,C   : in std_logic;
                      signal ir        : in std_logic_vector(31 downto 0)
                     ) return boolean is

   variable res       : std_logic;
   variable condition : BraOp;

   begin

      condition := ir_braOp(ir);

      case condition is
         when BraOp_BRA    =>  res :=  '1';              
         when BraOp_BCS    =>  res :=  C;                    -- BCS=BLO (unsigned)
         when BraOp_BEQ    =>  res :=  Z;                    -- (both)
         when BraOp_BVS    =>  res :=  V;                    -- (signed)
         when BraOp_BMI    =>  res :=  N;                    -- (signed)
         when BraOp_BLT    =>  res := (N xor V);             -- (signed)
         when BraOp_BLE    =>  res := (Z or (N xor V));      -- (signed)
         when BraOp_BLS    =>  res := (C or Z);              -- (unsigned)
         when BraOp_BSR    =>  res :=  '1';              
         when BraOp_BCC    =>  res := not C;                 -- BCC=BHS (unsigned)
         when BraOp_BNE    =>  res := not Z;                 -- (both)
         when BraOp_BVC    =>  res := not V;                 -- (signed)
         when BraOp_BPL    =>  res := not N;                 -- (signed)
         when BraOp_BGE    =>  res := not (N xor V);         -- (signed)
         when BraOp_BGT    =>  res := not (Z or (N xor V));  -- (signed)
         when BraOp_BHI    =>  res := not (C or Z);          -- (unsigned)
         when others =>  res := 'X';
      end case;

      return res = '1';

   end function doBranch;

   -- Needed for use in case statements
   variable irOp : IrOp;

   begin
      -- Default control signal values inactive
      -- Default selection for muxes
      loadIR        <= '0';

      loadPC        <= '0';
      pcSource      <= nextPC;

      regAWrite     <= '0';
      RegASource    <= aluOut;

      writeEn       <= '0';

      nextCpuState  <= fetch;

      irOp := ir_op(ir); -- extract opcode field
   
      case cpuState is

         when fetch => -- load next word into instruction register
            nextCpuState   <= decode;
            loadPC         <= '1';
            loadIR         <= '1';

         when decode =>
            case irOp is
               when IrOp_regReg =>                       -- Ra <- Rb op Rc
                  nextCpuState <= execute;
               when IrOp_RegImmed =>                     -- Ra <- Rb op sex(immed)
                  nextCpuState <= execute;
               when IrOp_LoadJump =>                     -- Ra <- mem(Rb + sex(immed))
                                                         -- PC <- Rb op sex(immed)
                  nextCpuState <= execute;
               when IrOp_Store =>                        -- mem(Rb + sex(immed)) <- Rc
                  nextCpuState <= execute;
               when IrOp_Branch =>
                  nextCpuState <= fetch;
                  if (ir_braOp(ir) = BraOp_BRA) then     -- PC <- PC + offset
                     loadPC       <= '1';
                     PCSource     <= branchPC;
                  elsif (ir_braOp(ir) = BraOp_BSR) then  -- PC <- PC + offset; Reg31 <- PC;
                     loadPC       <= '1';
                     PCSource     <= branchPC;
                  else                                   -- if (cond) PC <- PC + offset
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
               when IrOp_regReg | IrOp_RegImmed =>  -- Ra <- Rb op Rc, Ra <- Rb op sex(immed)
                  regAWrite    <= '1';            
                  nextCpuState <= fetch;
               when IrOp_LoadJump =>              
                  if (ir_regA(ir) /= "00000") then  -- Ra <- mem(Rb + sex(immed))
                     nextCpuState <= dataRead;
                  else                              -- PC <- Rb op sex(immed)
                     PCSource     <= jumpPC;
                     loadPC       <= '1';
                     nextCpuState <= fetch;
                  end if;
               when IrOp_Store =>                   -- mem(Rb + sex(immed)) <- Ra
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
