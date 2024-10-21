library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Multiplier5Cycle is
    Port ( Clock    : in  std_logic;
           Reset    : in  std_logic;
           A        : in  unsigned(15 downto 0);
           B        : in  unsigned(15 downto 0);
           Q        : out unsigned(31 downto 0);
           Start    : in  std_logic;
           Complete : out std_logic);
end Multiplier5Cycle;

architecture Behavioral of Multiplier5Cycle is

type theStates is (IdleState, Dummy1, Dummy2, Dummy3, Dummy4); 
signal state : theStates;

begin

   --
   -- Control state machine
   --
   StateMachine: -- State machine
   process (reset, clock)

   begin

      if (reset = '1') then
         state    <= IdleState;
         Complete <= '0';
         Q        <= (others => '0');
      elsif rising_edge(clock) then
         case state is
            when IdleState =>
               if (start = '1') then
                  state    <= Dummy1;
                  Complete <= '0';
                  Q        <= (others => 'X');
               else
                  state <= IdleState;
               end if;
            when Dummy1 =>
               state    <= Dummy2;
            when Dummy2 =>
               state    <= Dummy3;
            when Dummy3 =>
               state    <= Dummy4;
            when Dummy4 =>
               state    <= IdleState;
               Complete <= '1';
               Q <= A * B;
         end case;
      end if;
 
   end process StateMachine;

end architecture Behavioral;
