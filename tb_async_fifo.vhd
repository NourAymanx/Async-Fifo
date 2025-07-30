library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity tb_async_fifo is
end tb_async_fifo;

architecture behavioral of tb_async_fifo is
    constant DATA_WIDTH : positive := 8;
    constant ADDR_WIDTH : positive := 3;  -- FIFO depth = 8
    constant WCK_PERIOD : time := 10 ns;
    constant RCK_PERIOD : time := 23 ns;

    signal wck      : std_logic := '0';
    signal wr_rst_n : std_logic := '0';
    signal wr_en    : std_logic := '0';
    signal wr_data  : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
    signal full     : std_logic;

    signal rck      : std_logic := '0';
    signal rd_rst_n : std_logic := '0';
    signal rd_en    : std_logic := '0';
    signal rd_data  : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal empty    : std_logic;

    signal error_count : integer := 0;
begin
    uut: entity work.async_fifo
        generic map (
            DATA_WIDTH => DATA_WIDTH,
            ADDR_WIDTH => ADDR_WIDTH
        )
        port map (
            wck      => wck,
            wr_rst_n => wr_rst_n,
            wr_en    => wr_en,
            wr_data  => wr_data,
            full     => full,
            rck      => rck,
            rd_rst_n => rd_rst_n,
            rd_en    => rd_en,
            rd_data  => rd_data,
            empty    => empty
        );

    wck <= not wck after WCK_PERIOD/2;
    rck <= not rck after RCK_PERIOD/2;

    stimulus: process
        variable expected : std_logic_vector(DATA_WIDTH-1 downto 0);
    begin
        -- Initialize
        wr_rst_n <= '0';
        rd_rst_n <= '0';
        wait for 100 ns;
        
        -- Release resets
        wr_rst_n <= '1';
        rd_rst_n <= '1';
        wait for 50 ns;

        -- Test 1: Basic write/read
        report "Test 1: Basic write/read";
        for i in 0 to 5 loop
            wr_en <= '1';
            wr_data <= std_logic_vector(to_unsigned(i, DATA_WIDTH));
            wait until rising_edge(wck);
        end loop;
        wr_en <= '0';
        
        wait for 100 ns;  -- Allow sync
        
        for i in 0 to 5 loop
            rd_en <= '1';
            wait until rising_edge(rck);
            wait for 1 ps;  -- Small delta for data update
            expected := std_logic_vector(to_unsigned(i, DATA_WIDTH));
            if rd_data /= expected then
                report "Data mismatch! Expected " & integer'image(i) & 
                       " Got " & integer'image(to_integer(unsigned(rd_data)))
                    severity error;
                error_count <= error_count + 1;
            end if;
        end loop;
        rd_en <= '0';
        wait for 50 ns;
        if empty /= '1' then
            report "FIFO not empty!" severity error;
            error_count <= error_count + 1;
        end if;

        -- Test 2: Full condition
        report "Test 2: Full condition";
        wr_en <= '1';
        for i in 0 to 2**ADDR_WIDTH loop
            wr_data <= std_logic_vector(to_unsigned(i, DATA_WIDTH));
            wait until rising_edge(wck);
            if full = '1' then
                exit;
            end if;
        end loop;
        if full /= '1' then
            report "Full flag not set!" severity error;
            error_count <= error_count + 1;
        end if;
        wr_en <= '0';

        -- Test 3: Reset
        report "Test 3: Reset test";
        wr_rst_n <= '0';
        rd_rst_n <= '0';
        wait for 50 ns;
        wr_rst_n <= '1';
        rd_rst_n <= '1';
        wait for 50 ns;
        if empty /= '1' then
            report "FIFO not empty after reset!" severity error;
            error_count <= error_count + 1;
        end if;

        -- Final report
        if error_count = 0 then
            report "TEST PASSED" severity note;
        else
            report "TEST FAILED: " & integer'image(error_count) & " errors" severity error;
        end if;
        wait;
    end process;
end behavioral;
