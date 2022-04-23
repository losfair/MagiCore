module RvTop(
  input clk,
  input rst,
  output iBus_arvalid,
  input  iBus_arready,
  output [31:0] iBus_araddr,
  output [7:0] iBus_arlen,
  output [1:0] iBus_arburst,
  output [3:0] iBus_arcache,
  output [2:0] iBus_arprot,
  output [2:0] iBus_arsize, // added
  output iBus_arid, // added
  input iBus_rvalid,
  output iBus_rready,
  input [31:0] iBus_rdata,
  input [1:0] iBus_rresp,
  input iBus_rlast,
  input iBus_rid, // added
  output dBus_awvalid,
  input  dBus_awready,
  output [31:0] dBus_awaddr,
  output [7:0] dBus_awid,
  output [2:0] dBus_awsize, // added
  output [1:0] dBus_awburst, // added
  output [7:0] dBus_awlen, // added
  output dBus_wvalid,
  input  dBus_wready,
  output [31:0] dBus_wdata,
  output [3:0] dBus_wstrb,
  output dBus_wlast,
  input dBus_bvalid,
  output dBus_bready,
  input [7:0] dBus_bid,
  input [1:0] dBus_bresp,
  output dBus_arvalid,
  input  dBus_arready,
  output [31:0] dBus_araddr,
  output [7:0] dBus_arid,
  output [2:0] dBus_arsize, // added
  output [1:0] dBus_arburst, // added
  output [7:0] dBus_arlen, // added
  input dBus_rvalid,
  output dBus_rready,
  input [31:0] dBus_rdata,
  input [7:0] dBus_rid,
  input [1:0] dBus_rresp,
  input dBus_rlast
);

assign iBus_arsize = 3'b010; // 4 bytes per instruction
assign iBus_arid = 1'b0;

assign dBus_awsize = 3'b010;
assign dBus_awburst = 2'b01;
assign dBus_awlen = 8'b0;

assign dBus_arsize = 3'b010;
assign dBus_arburst = 2'b01;
assign dBus_arlen = 8'b0;

assign iBus_arprot = 3'b100; // cosim model bug

RiscvProcessor processor_1(
  .clk(clk),
  .reset(rst),
  .io_iBus_ar_valid(iBus_arvalid),
  .io_iBus_ar_ready(iBus_arready),
  .io_iBus_ar_payload_addr(iBus_araddr),
  .io_iBus_ar_payload_len(iBus_arlen),
  .io_iBus_ar_payload_burst(iBus_arburst),
  .io_iBus_ar_payload_cache(iBus_arcache),
  // .io_iBus_ar_payload_prot(iBus_arprot),
  .io_iBus_r_valid(iBus_rvalid),
  .io_iBus_r_ready(iBus_rready),
  .io_iBus_r_payload_data(iBus_rdata),
  .io_iBus_r_payload_resp(iBus_rresp),
  .io_iBus_r_payload_last(iBus_rlast),
  .io_dBus_aw_valid(dBus_awvalid),
  .io_dBus_aw_ready(dBus_awready),
  .io_dBus_aw_payload_addr(dBus_awaddr),
  .io_dBus_aw_payload_id(dBus_awid),
  .io_dBus_w_valid(dBus_wvalid),
  .io_dBus_w_ready(dBus_wready),
  .io_dBus_w_payload_data(dBus_wdata),
  .io_dBus_w_payload_strb(dBus_wstrb),
  .io_dBus_w_payload_last(dBus_wlast),
  .io_dBus_b_valid(dBus_bvalid),
  .io_dBus_b_ready(dBus_bready),
  .io_dBus_b_payload_id(dBus_bid),
  .io_dBus_b_payload_resp(dBus_bresp),
  .io_dBus_ar_valid(dBus_arvalid),
  .io_dBus_ar_ready(dBus_arready),
  .io_dBus_ar_payload_addr(dBus_araddr),
  .io_dBus_ar_payload_id(dBus_arid),
  .io_dBus_r_valid(dBus_rvalid),
  .io_dBus_r_ready(dBus_rready),
  .io_dBus_r_payload_data(dBus_rdata),
  .io_dBus_r_payload_id(dBus_rid),
  .io_dBus_r_payload_resp(dBus_rresp),
  .io_dBus_r_payload_last(dBus_rlast)
);

endmodule
