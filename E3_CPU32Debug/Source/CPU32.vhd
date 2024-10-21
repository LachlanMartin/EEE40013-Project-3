--******************************************************************
--  CPU.vhd
--******************************************************************

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

use work.cpupackage.ALL;
-- synopsys translate_off
use work.debug.ALL;
-- synopsys translate_on

entity cpu32 is
    Port ( reset     : in     std_logic;
           clock     : in     std_logic;
           pcOut     : out    std_logic_vector(31 downto 0);
			  
           -- I/O Port (for later project)
           pinIn   : in  std_logic_vector(15 downto 0);
           pinOut  : out std_logic_vector(15 downto 0);
           pinDrv  : out std_logic_vector(15 downto 0)
         );
end entity cpu32;
--
--************************************************************************
--
architecture Behavioral of cpu32 is

-- ALU signals
signal aluOp                : AluOp;
signal aluOperand1          : unsigned(31 downto 0);
signal aluOperand2          : unsigned(31 downto 0);
signal aluDataOut           : unsigned(31 downto 0);
signal Z,N,V,C              : std_logic;
signal doFlags              : std_logic;
signal aluStart             : std_logic;
signal aluComplete          : std_logic;

-- Instruction register
signal ir                  : std_logic_vector(31 downto 0);
signal loadIR              : std_logic;

-- Values calculated from IR
signal branchOffset        : unsigned(31 downto 0);

-- Registers and intermediate values
signal pc                  : std_logic_vector(31 downto 0);
signal loadPC              : std_logic;
signal PCSource            : PCSourceT;

-- Port A of Register file (write)
signal regAAddr            : std_logic_vector( 4 downto 0);
signal regAWrite           : std_logic;
signal regADataIn          : std_logic_vector(31 downto 0);
signal RegASource          : RegASourceT; 

-- Port B of Register file (read)
signal regBAddr            : std_logic_vector( 4 downto 0);
signal regBDataOut         : std_logic_vector(31 downto 0);

-- Port C of Register file (read)
signal regCAddr            : std_logic_vector( 4 downto 0);
signal regCDataOut         : std_logic_vector(31 downto 0);

-- Code Memory
signal codeMemAddr         : std_logic_vector(codeMemAddrWidth-1+2 downto 2);
signal codeMemDataOut      : std_logic_vector(31 downto 0);
signal codeMemRead         : std_logic;

-- Data Memory
signal dataMemAddr         : std_logic_vector(dataMemAddrWidth-1+2 downto 2);
signal dataMemWrite        : std_logic;
signal dataMemDataIn       : std_logic_vector(31 downto 0);
signal dataMemDataOut      : std_logic_vector(31 downto 0);

-- Memory, I/O signals
signal dataInBus           : std_logic_vector(31 downto 0);
signal dataOutBus          : std_logic_vector(31 downto 0);
signal addressBus          : std_logic_vector(31 downto 0);
signal writeEn             : std_logic;

-- Instruction opcode from IR
signal irOp : IrOp;

-- ADDED SIGNALS
signal ioPortWrite			: std_logic;
signal ioPortAddr				: std_logic_vector(2 downto 0);
signal ioPortDataIn        : std_logic_vector(15 downto 0);
signal ioPortDataOut       : std_logic_vector(15 downto 0);
--************************************************************************
begin
   
   -- Instruction opcode from IR
   irOp  <= ir_op(ir);
   
   pcOut <= pc(pcOut'range); -- to stop design being optimized away!

   -- For later expansion
   pinOut <= (others => '0');
   pinDrv <= (others => '0');
   
   --==============================================
   -- PC source data path
   --
   
   -- 32-bit signed-extended branch offset from ir
   branchOffset <= signExtend(branchOffset'length, ir_branchOffset(ir));

   pcProcess:
   process (reset, clock)
   begin
      if (reset = '1') then
         -- program execution starts from address $0000
         pc  <= (others => '0');
      elsif rising_edge(clock) then
         if (loadPC = '1') then
            case pcSource is
               when branchPC =>  pc <= std_logic_vector(unsigned(pc) + branchOffset);
               when nextPC   =>  pc <= std_logic_vector(unsigned(pc) + 4);
               when jumpPC   =>  pc <= std_logic_vector(aluDataOut);
            end case;
         end if;
       end if;
   end process;

   --==============================================
   -- Code Memory data path
   --
   codeMemAddr    <= PC(codeMemAddr'left downto 2); -- Note: Memory is 32-bit aligned
   ir             <= codeMemDataOut;
   
   codeMemRead    <= loadIR;

   --=============================================================
   -- Code Memory instantiation
   -- Word size (A1..0 not used)
   codeMem:
   entity work.codememory 
      Generic Map (
         dataWidth    => 32,
         addressWidth => codeMemAddrWidth
         )
      port map (
         clock => clock,
         addr  => codeMemAddr,
         dout  => codeMemDataOut,
         en    => codeMemRead
         );

   --=============================================
   -- Register File control
   --
   regControl:
   process (ir, RegASource, aluDataOut, dataInBus, pc, irOp)
   begin
      regAAddr   <= ir_regA(ir);
      regBAddr   <= ir_regB(ir);
      regCAddr   <= ir_regC(ir);

      case RegASource is
         when aluOut      => regADataIn <= std_logic_vector(aluDataOut);
         when dataMemOut  => regADataIn <= dataInBus;
         when reg31       => regAAddr   <= "11111";
                             regADataIn <= pc;
      end case;

   end process regControl;

   --=============================================================
   -- Register Bank instantiation
   --
   registerBank :         
   entity work.Registers
      Generic Map (
         dataWidth    => 32,
         addressWidth => 5
         )
      Port Map ( 
         reset     => reset,
         clock     => clock,
         -- Port A (write)
         writeA    => regAWrite,
         addressA  => regAAddr,
         dataInA   => regADataIn,
         -- Port B (read)
         addressB  => regBAddr,
         dataOutB  => regBDataOut,
         -- Port C (read)
         addressC  => regCAddr,
         dataOutC  => regCDataOut
         );

   --=============================================
   -- ALU operand source multiplexing
   -- ALU operation control
   --
   aluControl:
   process ( ir, regBDataOut, regCDataOut, irOp )

   variable signedImmediateValue   : unsigned(31 downto 0);
   variable unsignedImmediateValue : unsigned(31 downto 0);

   begin

      if (irOp = IrOp_RegReg) or (irOp = IrOp_RegImmed) then
         aluOp  <= ir_aluOp(ir);
      else
         aluOp  <= ALUop_Add; -- add for all other opcodes
      end if;

      -- Set up operand 1 to ALU
      aluOperand1 <= unsigned(regBDataOut);

      -- Set up operand 2 to ALU
      signedImmediateValue   := signExtend(signedImmediateValue'length,   ir_immediate16(ir));
      unsignedImmediateValue := zeroExtend(unsignedImmediateValue'length, ir_immediate16(ir));
      
      if (irOp = IrOp_RegReg) then
         aluOperand2  <= unsigned(regCDataOut);
      elsif (irOp = IrOp_RegImmed) then
         if (ir_aluOp(ir) = AluOp_Eor) then
            aluOperand2 <= signedImmediateValue;
         else  
            aluOperand2 <= unsignedImmediateValue;
         end if;
      else  
         -- 32-bit sign-extended immediate value from ir
         aluOperand2  <= signedImmediateValue;
      end if;

   end process aluControl;

   --=============================================================
   -- ALU instantiation
   --
   theALU:
   entity work.alu(Behavioral)
      Port Map (
         aluOp     => aluOp,
         operand1  => aluOperand1,
         operand2  => aluOperand2,
         aluOutput => aluDataOut,
         Z         => Z,
         N         => N,
         V         => V,
         C         => C,
         clock     => clock,
         reset     => reset,
         doFlags   => doFlags,
         aluComplete => aluComplete,
         aluStart    => aluStart
         );

   --================================================
   -- Data Memory and I/O data paths
   --

   -- Address bus connections
   addressBus     <= std_logic_vector(aluDataOut);
   dataMemAddr    <= addressBus(dataMemAddr'left downto 2); -- Note: Memory is 32-bit aligned
	ioPortAddr     <= addressBus(2 downto 0);
   -- DataOut bus connections
   dataOutBus     <= regCDataOut;
   dataMemDataIn  <= dataOutBus;
   ioPortDataIn   <= dataOutBus(15 downto 0);
   -- DataIn bus connections
	-- ADDED MUX
   dataInBus      <= dataMemDataOut when addressBus(15) = '0' else
							ioPortDataOut  when addressBus(15) = '1';
   -- Memory Selection
	-- ADDED ioPortWrite AND CHECK IF ADDRESSBUS 15
   dataMemWrite   <= writeEn and not addressBus(15);
	ioPortWrite    <= writeEn and     addressBus(15);
	-- ADDED IOPort
	-- IO Port instantiation
	theIOPort:
	entity work.IOPort
		port map (
			reset   => reset,			--
         clock   => clock,			--
         writeEn => ioPortWrite,
         addr    => ioPortAddr,
         dataIn  => ioPortDataIn,
         dataOut => ioPortDataOut,
         pinIn   => pinIn,
         pinOut  => pinOut,
         pinDrv  => pinDrv
		);
   -- Data Memory instantiation
   -- Word access (A1..0 not used)
   dataMem:
   entity work.datamemory 
      Generic Map (
         dataWidth    => 32,
         addressWidth => dataMemAddrWidth
         )
      port map (
         clock => clock,
         addr  => dataMemAddr,
         din   => dataMemDataIn,
         dout  => dataMemDataOut,
         we    => dataMemWrite
         );
   
   --=============================================================
   -- Controller state machine
   --
   theController:
   entity work.Control
      Port map (
         reset        => reset,
         clock        => clock,
         ir           => ir,
         Z => Z, N => N, V => V, C => C, 
         regAWrite    => regAWrite,
         RegASource   => RegASource, 
         loadPC       => loadPC,
         loadIR       => loadIR,
         writeEn      => writeEn,
         PCSource     => PCSource,
         doFlags      => doFlags,
         aluStart     => aluStart,
         aluComplete  => aluComplete
         );

end architecture Behavioral;
