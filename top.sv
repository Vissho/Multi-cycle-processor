module regfile(
	input logic clk,
	input logic we3,
	input logic [4:0] ra1, ra2, wa3,
	input logic [31:0] wd3,
	output logic [31:0] rd1, rd2
	);
	
	logic [31:0] rf[31:0];
	
	// three ported register file
	// read two ports combinationally
	// write third port on rising edge of clk
	// register 0 hardwired to 0
	// note: for pipelined processor, write third port
	// on falling edge of clk
	
	always_ff @(posedge clk)
		if (we3) rf[wa3] <= wd3;
	
	assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
	assign rd2 = (ra2 != 0) ? rf[ra2] : 0;
	
endmodule

module adder(
	input logic [31:0] a, b,
	output logic [31:0] y
	);
	
	assign y = a + b;
	
endmodule 

module sl2(
	input logic [31:0] a,
	output logic [31:0] y
	);
	
	// shift left by 2
	assign y = {a[29:0], 2'b00};
	
endmodule

module signext(
	input logic [15:0] a,
	output logic [31:0] y
	);
	
	assign y = {{16{a[15]}}, a};
	
endmodule

module flopr #(parameter WIDTH = 8)(
	input logic clk, reset,
	input logic [WIDTH-1:0] d,
	output logic [WIDTH-1:0] q
	);
	
	always_ff @(posedge clk, posedge reset)
		if (reset)
			q <= 0;
		else
			q <= d;
	
endmodule 

module flopre #(parameter WIDTH = 8)(
	input logic clk, reset, enable,
	input logic [WIDTH-1:0] d,
	output logic [WIDTH-1:0] q
	);
	
	always_ff @(posedge clk, posedge reset)
		if (reset)
			q <= 0;
		else if (enable)
			q <= d;
	
endmodule 

module mux2 #(parameter WIDTH = 8)(
	input logic [WIDTH-1:0] d0, d1,
	input logic s,
	output logic [WIDTH-1:0] y
	);
	
	assign y = s ? d1 : d0;
	
endmodule

module pcsrcmux #(parameter WIDTH = 8)(
	input logic [WIDTH-1:0] d0, d1, d2,
	input logic [1:0] s,
	output logic [WIDTH-1:0] y
	);
	
	always_comb
	case (s)
		2'b00: y <= d0;
		2'b01: y <= d1;
		2'b10: y <= d2;
		default: y <= {WIDTH{1'bx}};
	endcase
	
endmodule

module mux4 #(parameter WIDTH = 8)(
	input logic [WIDTH-1:0] d00, d01, d10, d11,
	input logic [1:0] s,
	output logic [WIDTH-1:0] y
	);
	
	always_comb
		case(s)
			2'b00:	y <= d00;
			2'b01:	y <= d01;
			2'b10:	y <= d10;
			2'b11:	y <= d11;
			default:	y <= {WIDTH{1'bx}};
		endcase
	
endmodule

module alu #(parameter WIDTH = 4)(
	input logic [WIDTH-1:0] operandA, operandB,
	input logic [2:0] funcsel,
	output logic [WIDTH-1:0] result,
	output logic zero
	);
	
	logic [WIDTH-1:0] maybeinvertedB;
	assign maybeinvertedB = funcsel[2] ? ~operandB : operandB;
	
	logic [WIDTH-1:0] sum;
	assign sum = operandA + maybeinvertedB + funcsel[2];
	
	always_comb
		case(funcsel[1:0])
			2'b00: result <= operandA & maybeinvertedB;
			2'b01: result <= operandA | maybeinvertedB;
			2'b10: result <= sum;
			2'b11: result <= {{WIDTH-1{1'b0}}, sum[WIDTH-1]};
		endcase
	
	assign zero = (result == {WIDTH{1'b0}}) ? 1'b1 : 1'b0 ;
	
endmodule

module maindec(
	input logic clk, reset,
	input logic [5:0] op,
	output logic memwrite,
	output logic pc_we, iord, ir_we, regwrite, alusrca,
	output logic [1:0] alusrcb,
	output logic [1:0] aluop,
	output logic regdst, memtoreg,
	output logic [1:0] pcsrc,
	output logic branch
	);
	
	typedef enum logic [3:0] {
		S0_Fetch,
		S1_Decode,
		S2_MemAdr,
		S3_MemRead,
		S4_MemWriteback,
		S5_MemWrite,
		S6_Execute,
		S7_ALUWriteback,
		S8_Branch,
		S9_ADDIExecute,
		S10_ADDIWriteback,
		S11_Jump
	} statetype;

	statetype state, nextstate;
	
	always_ff @(posedge clk, posedge reset)
		if (reset) state <= S0_Fetch;
		else state <= nextstate;
		
	// next state logic
	always_comb begin
		case (state)
			S0_Fetch: nextstate <= S1_Decode;
			S1_Decode: begin
				case (op)
					6'b100011: nextstate <= S2_MemAdr;
					6'b101011: nextstate <= S2_MemAdr;
					6'b000000: nextstate <= S6_Execute;
					6'b000100: nextstate <= S8_Branch;
					6'b001000: nextstate <= S9_ADDIExecute;
					6'b000010: nextstate <= S11_Jump;
					default: nextstate <= S0_Fetch;
				endcase
			end
			S2_MemAdr: begin
				case (op)
					6'b100011: nextstate <= S3_MemRead;
					6'b101011: nextstate <= S5_MemWrite;
					default: nextstate <= S0_Fetch;
				endcase
			end
			S3_MemRead: nextstate <= S4_MemWriteback;
			S4_MemWriteback: nextstate <= S0_Fetch;
			S5_MemWrite: nextstate <= S8_Branch;
			S6_Execute: nextstate <= S7_ALUWriteback;
			S7_ALUWriteback: nextstate <= S0_Fetch;
			S8_Branch: nextstate <= S0_Fetch;
			S9_ADDIExecute: nextstate <= S10_ADDIWriteback;
			S10_ADDIWriteback: nextstate <= S0_Fetch;
			S11_Jump: nextstate <= S0_Fetch;
			default: nextstate <= S0_Fetch;
		endcase
		end
	
	logic [14:0] controls;
	
	assign {memwrite, pc_we, iord, ir_we, regwrite, alusrca, alusrcb, aluop, regdst, memtoreg, pcsrc, branch} = controls;
	
	always_comb
		case(state)
			S0_Fetch: controls <= 15'b010100_01_00_xx00_x;
			S1_Decode: controls <= 15'b000000_11_00_0000_0;
			S2_MemAdr: controls <= 15'b000001_10_00_0000_0;
			S3_MemRead: controls <= 15'b001000_00_00_0000_0;
			S4_MemWriteback: controls <= 15'b000010_00_00_0100_0;
			S5_MemWrite: controls <= 15'b101000_00_00_0000_0;
			S6_Execute: controls <= 15'b000001_00_10_0000_0;
			S7_ALUWriteback: controls <= 15'b000010_00_00_1000_0;
			S8_Branch: controls <= 15'b000001_00_01_0001_1;
			S9_ADDIExecute: controls <= 15'b000001_10_00_0000_0;
			S10_ADDIWriteback: controls <= 15'b000010_00_00_0000_0;
			S11_Jump: controls <= 15'b010000_00_00_0010_0;
			default: controls <= 15'bxxxxxxxxxxxxxxx;
		endcase
		
endmodule

module aludec(
	input logic [5:0] funct,
	input logic [1:0] aluop,
	output logic [2:0] alucontrol
	);
	
	always_comb
		case(aluop)
			2'b00: alucontrol <= 3'b010; // add (for lw/sw/addi)
			2'b01: alucontrol <= 3'b110; // sub (for beq)
			default: 
				case(funct) // R-type instructions
					6'b100000: alucontrol <= 3'b010; // add
					6'b100010: alucontrol <= 3'b110; // sub
					6'b100100: alucontrol <= 3'b000; // and
					6'b100101: alucontrol <= 3'b001; // or
					6'b101010: alucontrol <= 3'b111; // slt
					default: alucontrol <= 3'bxxx; // ???
				endcase
		endcase
endmodule

module controller(
	input clk, reset,
	input logic [5:0] op, funct,
	input logic zero,
	output logic memwrite,
	output logic pc_we, iord, ir_we, regwrite, alusrca, 
	output logic [1:0] alusrcb,
	output logic [2:0] alucontrol,
	output logic regdst, memtoreg,
	output logic [1:0] pcsrc,
	output logic branch
	);
	
	logic [1:0] aluop;
	
	maindec md(
		clk, reset, op, memwrite,
		pc_we, iord, ir_we, regwrite, alusrca, alusrcb, aluop,
		regdst, memtoreg, pcsrc, branch);
	
	aludec ad(funct, aluop, alucontrol);
	
endmodule

module datapath(
	input logic clk, reset,
	output logic [31:0] instr,
	input logic pcwrite, iord, ir_we, regwrite,
	input logic alusrca,
	input logic [1:0] alusrcb,
	input logic [2:0] alucontrol,
	input logic regdst, memtoreg,
	input logic [1:0] pcsrc, 
	input logic branch,
	output logic zero,
	output logic [31:0] adr, writedata,
	input  logic [31:0] readdata
	);
	
	logic [31:0] pc, aluout, aluresult, data;
	logic [31:0] rd1, rd2, data_a, signimm, srca, srcb, shiftedsignimm, newpc;
	logic [4:0]  regwriteadr;
	logic [31:0] regwritedata;
	logic pc_we;
	logic [31:0] PCJump;
	
	assign pc_we = pcwrite | (branch & zero);
	assign PCJump [25:0] = instr [25:0];
	assign PCJump [27:26] = PCJump [25:00] << 2;
	assign PCJump [31:28] = pc [31:28];
		
	flopre #(32) pc_reg(clk, reset, pc_we, newpc, pc);
	mux2   #(32) memadrmux(pc, aluout, iord, adr);
	flopre #(32) inst_reg(clk, reset, ir_we, readdata, instr);
	flopr  #(32) data_reg(clk, reset, readdata, data);
	mux2   #(5) regdst_mux(instr[20:16], instr[15:11], regdst, regwriteadr);
	mux2   #(32) memtoreg_mux(aluout, data, memtoreg, regwritedata);
	regfile		 rf(clk, regwrite, instr[25:21], instr[20:16], regwriteadr, regwritedata, rd1, rd2);
	flopr  #(32) rd1_reg(clk, reset, rd1, data_a);
	flopr  #(32) rd2_reg(clk, reset, rd2, writedata);
	signext		 se(instr[15:0], signimm);
	mux2   #(32) srca_mux(pc, data_a, alusrca, srca);
	mux4   #(32) srcb_mux(writedata, 4, signimm, shiftedsignimm, alusrcb, srcb);
	alu    #(32) alu(srca, srcb, alucontrol, aluresult, zero);
	flopr  #(32) aluout_reg(clk, reset, aluresult, aluout);
	sl2			 sl2(signimm, shiftedsignimm);
	pcsrcmux   #(32) pcsrc_mux(aluresult, aluout, PCJump, pcsrc, newpc);
	
endmodule


module mips(
	input logic clk, reset,
	output logic memwrite,
	output logic [31:0] adr, writedata, 
	input logic [31:0] readdata
	);
	
	logic pc_we, iord, ir_we, regwrite, alusrca, zero;
	logic [2:0] alucontrol;
	logic [1:0] alusrcb;
	logic regdst, memtoreg, branch; 
	logic [1:0] pcsrc;
	logic [31:0] instr;
	
	controller c( 
		clk, reset,
		instr[31:26], instr[5:0], zero, memwrite,
		pc_we, iord, ir_we, regwrite, alusrca, alusrcb,
		alucontrol, regdst, memtoreg, pcsrc, branch);
	
	datapath dp(
		clk, reset, instr,
		pc_we, iord, ir_we, regwrite, alusrca, alusrcb,
		alucontrol, regdst, memtoreg, pcsrc, branch, zero, adr, writedata, readdata);
	
endmodule


module dmem(
	input logic clk, we,
	input logic [31:0] a, wd,
	output logic [31:0] rd
	);
		
	logic [31:0] RAM[63:0];
	
	initial
		$readmemh("memfile.dat", RAM);
	
	assign rd = RAM[a[31:2]]; // word aligned
	
	always_ff @(posedge clk)
		if (we) RAM[a[31:2]] <= wd;
		
endmodule


module top(
	input logic clk, reset,
	output logic [31:0] writedata, dataadr,
	output logic memwrite
	);
	
	logic [31:0] readdata;
	
	mips mips1(clk, reset, memwrite, dataadr, writedata, readdata);
	dmem dmem1(clk, memwrite, dataadr, writedata, readdata);
	
endmodule
