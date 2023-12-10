module test_processor;
  logic clk = 0, reset = 1, memwrite; 
  logic [6:0] count = 0;
  logic [31:0] writedata, dataadr; 
  
  top name (.clk(clk), .reset(reset), .writedata(writedata),
	.dataadr(dataadr), .memwrite(memwrite));

  always #5 clk = ~clk;
  always @(posedge clk) begin
	if (count == 1) begin
		reset = 0;
	end
	if (count == 50) begin
		$stop;
	end
	count = count + 1;
  end
  

endmodule