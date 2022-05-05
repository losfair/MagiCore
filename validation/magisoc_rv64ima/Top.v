module Top(
  input clk,
  input rst,
  output dBus_awvalid,
  input  dBus_awready,
  output [31:0] dBus_awaddr,
  output [15:0] dBus_awid,
  output [2:0] dBus_awsize, // added
  output [1:0] dBus_awburst, // added
  output [7:0] dBus_awlen, // added
  output dBus_wvalid,
  input  dBus_wready,
  output [63:0] dBus_wdata,
  output [7:0] dBus_wstrb,
  output dBus_wlast,
  input dBus_bvalid,
  output dBus_bready,
  input [15:0] dBus_bid,
  input [1:0] dBus_bresp,
  output dBus_arvalid,
  input  dBus_arready,
  output [31:0] dBus_araddr,
  output [15:0] dBus_arid,
  output [2:0] dBus_arsize, // added
  output [1:0] dBus_arburst, // added
  output [7:0] dBus_arlen, // added
  input dBus_rvalid,
  output dBus_rready,
  input [63:0] dBus_rdata,
  input [15:0] dBus_rid,
  input [1:0] dBus_rresp,
  input dBus_rlast,

  output uart_txd,
  input uart_rxd,

  input core_interrupts
);

MagiSoC_RV64IMA soc_1(
  .clk(clk),
  .reset(rst),
  .io_bus_aw_valid(dBus_awvalid),
  .io_bus_aw_ready(dBus_awready),
  .io_bus_aw_payload_addr(dBus_awaddr),
  .io_bus_aw_payload_id(dBus_awid),
  //.io_bus_aw_payload_region(0),
  .io_bus_aw_payload_len(dBus_awlen),
  .io_bus_aw_payload_size(dBus_awsize),
  .io_bus_aw_payload_burst(dBus_awburst),
  //.io_bus_aw_payload_lock(0),
  //.io_bus_aw_payload_cache(0),
  //.io_bus_aw_payload_qos(0),
  //.io_bus_aw_payload_prot(0),
  .io_bus_w_valid(dBus_wvalid),
  .io_bus_w_ready(dBus_wready),
  .io_bus_w_payload_data(dBus_wdata),
  .io_bus_w_payload_strb(dBus_wstrb),
  .io_bus_w_payload_last(dBus_wlast),
  .io_bus_b_valid(dBus_bvalid),
  .io_bus_b_ready(dBus_bready),
  .io_bus_b_payload_id(dBus_bid),
  .io_bus_b_payload_resp(dBus_bresp),
  .io_bus_ar_valid(dBus_arvalid),
  .io_bus_ar_ready(dBus_arready),
  .io_bus_ar_payload_addr(dBus_araddr),
  .io_bus_ar_payload_id(dBus_arid),
  //.io_bus_ar_payload_region(0),
  .io_bus_ar_payload_len(dBus_arlen),
  .io_bus_ar_payload_size(dBus_arsize),
  .io_bus_ar_payload_burst(dBus_arburst),
  //.io_bus_ar_payload_lock(0),
  //.io_bus_ar_payload_cache(0),
  //.io_bus_ar_payload_qos(0),
  //.io_bus_ar_payload_prot(0),
  .io_bus_r_valid(dBus_rvalid),
  .io_bus_r_ready(dBus_rready),
  .io_bus_r_payload_data(dBus_rdata),
  .io_bus_r_payload_id(dBus_rid),
  .io_bus_r_payload_resp(dBus_rresp),
  .io_bus_r_payload_last(dBus_rlast),
  .io_uart_txd(uart_txd),
  .io_uart_rxd(uart_rxd),
  .io_interrupts(core_interrupts)
);

endmodule
