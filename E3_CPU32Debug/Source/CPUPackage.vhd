--================================================================
--	CPUPackage.vhd
--
--	Purpose: This package defines supplemental types, subtypes, 
--		      constants, and functions for the CPU design

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

package cpupackage is

   -- Memory dimensions (width of address bus determines memory size in words)
   constant dataMemAddrWidth : natural := 9; -- 512 words
   constant codeMemAddrWidth : natural := 8; -- 256 words

   -- ALU operation control field from the Instruction Register.
   type AluOp is (
      AluOp_Add,
      AluOp_Sub,
      AluOp_And,
      AluOp_Or,
      AluOp_Eor,
      AluOp_Swap,
      AluOp_Ror,
      AluOp_Mul
   ); 

   -- Extract ALU operation control field from the Instruction Register.
   function ir_aluOp ( signal ir : in std_logic_vector(31 downto 0)) return AluOp;

   -- Branch control field from the Instruction Register.
   type BraOp is (
       BraOp_BRA, BraOp_BSR, 
       BraOp_BCS, BraOp_BCC, -- BCS=BLO / BCC=BHS
       BraOp_BEQ, BraOp_BNE,
       BraOp_BVS, BraOp_BVC,
       BraOp_BMI, BraOp_BPL,
       BraOp_BLT, BraOp_BGE,
       BraOp_BLE, BraOp_BGT,
       BraOp_BLS, BraOp_BHI
   );

   -- Extract branch control field from the Instruction Register.
   function ir_braOp ( signal ir : in std_logic_vector(31 downto 0)) return BraOp;

   -- Main IR opcode field from the Instruction Register.
   type IrOp is (
       IrOp_RegReg,     -- RegA <- RegB op RegC
       IrOp_RegImmed,   -- RegA <- RegB op sex/zex(Immed)
       IrOp_LoadJump,   -- RegA <- mem(RegB + sex(Immed)), PC <- RegB + sex(Offset)
       IrOp_Store,      -- mem(RegB + sex(Offset)) <- RegC
       IrOp_Branch      -- BRA/BSR/Bcc
   );

   -- Extract main IR opcode field from the Instruction Register.
   function ir_op ( signal ir : in std_logic_vector(31 downto 0)) return IrOp;

   -- Extract instruction size from the Instruction Register (not used).
   function ir_size           ( signal ir : in std_logic_vector(31 downto 0))     return std_logic_vector;

   -- Extract Register port A address from the Instruction Register
   function ir_regA           ( signal ir : in std_logic_vector(31 downto 0))     return std_logic_vector;

   -- Extract Register port B address from the Instruction Register
   function ir_regB           ( signal ir : in std_logic_vector(31 downto 0))     return std_logic_vector;

   -- Extract Register port C address from the Instruction Register
   function ir_regC           ( signal ir : in std_logic_vector(31 downto 0))     return std_logic_vector;

   -- Extract 16-bit immediate value from the Instruction Register
   function ir_immediate16    ( signal ir : in std_logic_vector(31 downto 0))     return unsigned;

   -- Extract 23-bit branch offset value from the Instruction Register.
   function ir_branchOffset   ( signal ir : in std_logic_vector(31 downto 0))     return unsigned;

   -- Function to do sign extension
   function signExtend ( constant toWidth : in integer;
                  constant data    : in unsigned)
            return unsigned;

   -- Function to do zero extension
   function zeroExtend ( constant toWidth : in integer;
                  constant data    : in unsigned)
            return unsigned;

   -- Type for PC source control
   type   PCSourceT is (branchPC, nextPC, jumpPC); 

   -- Type for Register write port source control
   type   RegASourceT is (aluOut,dataMemOut); 

end package cpupackage;

--*******************************************************************
-- Implementation of above
--
package body cpupackage is

   --================================================
   function ir_op( signal ir : in std_logic_vector(31 downto 0))
            return IrOp is
   begin   
      return IrOp'val(to_integer(unsigned(ir(31 downto 29))));
   end function;

   --================================================
   function ir_aluOp( signal ir : in std_logic_vector(31 downto 0))
            return AluOp is
   begin   
      return AluOp'val(to_integer(unsigned(ir(28 downto 26))));
   end function;

   --================================================
   function ir_braOp( signal ir : in std_logic_vector(31 downto 0))
            return BraOp is
   begin   
      return BraOp'val(to_integer(unsigned(ir(26 downto 23))));
   end function;

   --================================================
   function ir_size( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      return ir(27 downto 26);
   end function;

   --================================================
   function ir_regA( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      return ir(25 downto 21);
   end function ir_regA;

   --================================================
   function ir_regB( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      return ir(20 downto 16);
   end function;

   --================================================
   function ir_regC( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin  
      if (ir_op(ir) /= IrOp_Store) then
         return ir(15 downto 11);
      else
         -- The Register C address field in different position for Store 
         return ir(25 downto 21);
      end if;
   end function;

   --================================================
   function ir_immediate16( signal ir : in std_logic_vector(31 downto 0))
            return unsigned is
   begin   
      return unsigned(ir(15 downto  0));
   end function;

   --================================================
   function ir_branchOffset( signal ir : in std_logic_vector(31 downto 0))
            return unsigned is
            
   variable temp : unsigned(24 downto 0);

   begin   
      temp := unsigned(ir(22 downto  0)&"00");
      return temp;
   end function;

   --================================================
   function signExtend ( constant toWidth : in integer;
                  constant data    : in unsigned)
            return unsigned is

   variable toVector : unsigned(toWidth-1 downto 0);

   begin
      toVector(toWidth-1 downto data'left) := (others => data(data'left));
      toVector(data'left downto 0)         := data;
      return toVector;
   end function signExtend;

   --================================================
   function zeroExtend ( constant toWidth : in integer;
                  constant data    : in unsigned)
            return unsigned is

   variable toVector : unsigned(toWidth-1 downto 0);

   begin
      toVector(toWidth-1 downto data'left) := (others => '0');
      toVector(data'left downto 0)         := data;
      return toVector;
   end function zeroExtend;

end package body cpupackage;
