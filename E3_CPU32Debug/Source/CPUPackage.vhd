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
constant ALUopAdd     : std_logic_vector := "000";
constant ALUopSub     : std_logic_vector := "001";
constant ALUopAnd     : std_logic_vector := "010";
constant ALUopOr      : std_logic_vector := "011";
constant ALUopEor     : std_logic_vector := "100";
constant ALUopSwap    : std_logic_vector := "101";
constant ALUopROR     : std_logic_vector := "110";
constant ALUopMult    : std_logic_vector := "111";

-- Branch control field from the Instruction Register.
constant BRA : std_logic_vector := "000"; -- BRA / BSR
constant BCS : std_logic_vector := "001"; -- BCS=BLO / BCC=BHS
constant BEQ : std_logic_vector := "010"; -- BEQ / BNE
constant BVS : std_logic_vector := "011"; -- BVS / BVC
constant BMI : std_logic_vector := "100"; -- BMI / BPL
constant BLT : std_logic_vector := "101"; -- BLT / BGE
constant BLE : std_logic_vector := "110"; -- BLE / BGT
constant BLS : std_logic_vector := "111"; -- BLS / BHI

-- Functions to extract fields from Instruction Register.
function ir_op             ( signal ir : in std_logic_vector(31 downto 0))
         return std_logic_vector;
function ir_aluOp          ( signal ir : in std_logic_vector(31 downto 0))
         return std_logic_vector;
function ir_size           ( signal ir : in std_logic_vector(31 downto 0))
         return std_logic_vector;
function ir_regA           ( signal ir : in std_logic_vector(31 downto 0))
         return std_logic_vector;
function ir_regB           ( signal ir : in std_logic_vector(31 downto 0))
         return std_logic_vector;
function ir_regC           ( signal ir : in std_logic_vector(31 downto 0))
         return std_logic_vector;
function ir_immediate16    ( signal ir : in std_logic_vector(31 downto 0))
         return unsigned;
function ir_branchCondition( signal ir : in std_logic_vector(31 downto 0))
         return std_logic_vector;
function ir_branchOffset   ( signal ir : in std_logic_vector(31 downto 0))
         return unsigned;

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
type   RegASourceT is (aluOut,dataMemOut,reg31); 

end package cpupackage;

--*******************************************************************
-- Implementation of above
--
package body cpupackage is

   function ir_op( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      return ir(31 downto 29);
   end function ir_op;

   function ir_aluOp( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      return ir(28 downto 26);
   end function ir_aluOp;

   function ir_size( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      return ir(27 downto 26);
   end function ir_size;

   function ir_regA( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      return ir(25 downto 21);
   end function ir_regA;

   function ir_regB( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      return ir(20 downto 16);
   end function ir_regB;

   function ir_regC( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      if (ir_op(ir) /= "011") then
         return ir(15 downto 11);
      else
         return ir(25 downto 21);
      end if;
   end function ir_regC;

   function ir_immediate16( signal ir : in std_logic_vector(31 downto 0))
            return unsigned is
   begin   
      return unsigned(ir(15 downto  0));
   end function ir_immediate16;

   function ir_branchCondition( signal ir : in std_logic_vector(31 downto 0))
            return std_logic_vector is
   begin   
      return ir(26 downto 23);
   end function ir_branchCondition;

   function ir_branchOffset( signal ir : in std_logic_vector(31 downto 0))
            return unsigned is
            
   variable temp : unsigned(24 downto 0);

   begin   
      temp := unsigned(ir(22 downto  0)&"00");
      return temp;
   end function ir_branchOffset;

   function signExtend ( constant toWidth : in integer;
                  constant data    : in unsigned)
            return unsigned is

   variable toVector : unsigned(toWidth-1 downto 0);

   begin
      toVector(toWidth-1 downto data'left) := (others => data(data'left));
      toVector(data'left downto 0)         := data;

      return toVector;

   end function signExtend;

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