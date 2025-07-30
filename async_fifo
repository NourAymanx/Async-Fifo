library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity async_fifo is
    generic (
        DATA_WIDTH : positive := 8;
        ADDR_WIDTH : positive := 3  -- FIFO depth = 2^ADDR_WIDTH
    );
    port (
        -- Write domain ports
        wck      : in  std_logic;
        wr_rst_n : in  std_logic;
        wr_en    : in  std_logic;
        wr_data  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        full     : out std_logic;
        
        -- Read domain ports
        rck      : in  std_logic;
        rd_rst_n : in  std_logic;
        rd_en    : in  std_logic;
        rd_data  : out std_logic_vector(DATA_WIDTH-1 downto 0);
        empty    : out std_logic
    );
end async_fifo;

architecture behavioral of async_fifo is
    type mem_type is array (0 to 2**ADDR_WIDTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal mem : mem_type;

    -- Gray code conversion functions
    function bin2gray(bin: unsigned) return unsigned is
    begin
        return bin xor ('0' & bin(bin'high downto 1));
    end function;

    function gray2bin(gray: unsigned) return unsigned is
        variable bin: unsigned(gray'range);
    begin
        bin(bin'high) := gray(gray'high);
        for i in gray'high-1 downto 0 loop
            bin(i) := bin(i+1) xor gray(i);
        end loop;
        return bin;
    end function;

    -- Write domain signals
    signal b_wptr      : unsigned(ADDR_WIDTH downto 0) := (others => '0');
    signal b_wptr_next : unsigned(ADDR_WIDTH downto 0);
    signal g_wptr      : unsigned(ADDR_WIDTH downto 0) := (others => '0');
    signal g_wptr_next : unsigned(ADDR_WIDTH downto 0);
    signal full_i      : std_logic;
    
    -- Read domain signals
    signal b_rptr      : unsigned(ADDR_WIDTH downto 0) := (others => '0');
    signal b_rptr_next : unsigned(ADDR_WIDTH downto 0);
    signal g_rptr      : unsigned(ADDR_WIDTH downto 0) := (others => '0');
    signal g_rptr_next : unsigned(ADDR_WIDTH downto 0);
    signal empty_i     : std_logic;
    
    -- Synchronization signals
    signal g_rptr_sync1, g_rptr_sync2 : unsigned(ADDR_WIDTH downto 0) := (others => '0');
    signal g_wptr_sync1, g_wptr_sync2 : unsigned(ADDR_WIDTH downto 0) := (others => '0');
    signal b_rptr_sync : unsigned(ADDR_WIDTH downto 0);
    signal b_wptr_sync : unsigned(ADDR_WIDTH downto 0);
    
begin
    -- Convert synchronized gray pointers to binary
    b_rptr_sync <= gray2bin(g_rptr_sync2);
    b_wptr_sync <= gray2bin(g_wptr_sync2);

    -- Next pointer values (fixed syntax)
    b_wptr_next <= b_wptr + 1 when (wr_en = '1' and full_i = '0') else b_wptr;
    g_wptr_next <= bin2gray(b_wptr_next);
    b_rptr_next <= b_rptr + 1 when (rd_en = '1' and empty_i = '0') else b_rptr;
    g_rptr_next <= bin2gray(b_rptr_next);

    -- Full condition (write domain)
    full_i <= '1' when ((b_wptr(ADDR_WIDTH) /= b_rptr_sync(ADDR_WIDTH)) and 
                       (b_wptr(ADDR_WIDTH-1 downto 0) = b_rptr_sync(ADDR_WIDTH-1 downto 0))) 
              else '0';
    full <= full_i;

    -- Empty condition (read domain)
    empty_i <= '1' when (b_rptr = b_wptr_sync) else '0';
    empty <= empty_i;

    -- Write domain process
    write_process: process(wck, wr_rst_n)
    begin
        if wr_rst_n = '0' then
            b_wptr <= (others => '0');
            g_wptr <= (others => '0');
            g_rptr_sync1 <= (others => '0');
            g_rptr_sync2 <= (others => '0');
        elsif rising_edge(wck) then
            -- Synchronize read pointer
            g_rptr_sync1 <= g_rptr;
            g_rptr_sync2 <= g_rptr_sync1;
            
            -- Write operation
            if wr_en = '1' and full_i = '0' then
                mem(to_integer(b_wptr(ADDR_WIDTH-1 downto 0))) <= wr_data;
                b_wptr <= b_wptr_next;
                g_wptr <= g_wptr_next;
            end if;
        end if;
    end process;

    -- Read domain process
    read_process: process(rck, rd_rst_n)
    begin
        if rd_rst_n = '0' then
            b_rptr <= (others => '0');
            g_rptr <= (others => '0');
            g_wptr_sync1 <= (others => '0');
            g_wptr_sync2 <= (others => '0');
            rd_data <= (others => '0');
        elsif rising_edge(rck) then
            -- Synchronize write pointer
            g_wptr_sync1 <= g_wptr;
            g_wptr_sync2 <= g_wptr_sync1;
            
            -- Read operation
            if rd_en = '1' and empty_i = '0' then
                rd_data <= mem(to_integer(b_rptr(ADDR_WIDTH-1 downto 0)));
                b_rptr <= b_rptr_next;
                g_rptr <= g_rptr_next;
            end if;
        end if;
    end process;
end behavioral;
