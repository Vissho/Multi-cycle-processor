module ProcessorPrototype(
  input logic clk,
  input logic reset
); // 

  logic [31:0] ALUResult, SignImm, SrcA, SrcB, WriteData, Instr, ReadData, PC;
  logic Zero;
  
  logic MemtoReg, MemWrite, Branch, ALUSrc, RegDst, RegWrite, Jump;
  logic [2:0] ALUControl;

  logic [4:0] WriteReg;
  
  logic PCSrc;
  
  logic [31:0] Result;
  
  logic [31:0] Next, PCn, PCPlus4, PCJump, SignImmOffset, PCBranch;
  
  always_ff @(posedge clk) begin
		if (reset)
			PC <= 0;
		else
			PC <= PCn;
		PCPlus4 <= PC + 4;
  end
  
  always_comb begin

		PCJump [25:0] <= Instr [25:0];
		PCJump [27:26] <= PCJump [25:00] << 2;
		PCJump [31:28] <= PCPlus4 [31:28];
		
		SignImmOffset <= SignImm << 2;
		PCBranch <= SignImmOffset + PCPlus4;
	 
	 if (RegDst)
		WriteReg <= Instr [15:11];
	 else
		WriteReg <= Instr [20:16];
		
	 if (ALUSrc)
		SrcB <= SignImm;
	 else
		SrcB <= WriteData;
		
	 if (MemtoReg)
		Result <= ReadData;
	 else
		Result <= ALUResult;
		
	 if (PCSrc)
		Next <= PCBranch;
	 else
		Next <= PCPlus4;
		
	 if (Jump)
		PCn <= PCJump;
	 else
		PCn <= Next;
  end
  
  assign PCSrc = Branch & Zero;
  
  
  InstructionMemory instrMem(
    .A(PC),
    .RD(Instr)
  );
  
  ControlUnit ctrlUnit(
	 .Jump(Jump),
	 .Op(Instr [31:26]),
	 .Funct(Instr [5:0]),
	 .MemtoReg(MemtoReg),
	 .MemWrite(MemWrite),
	 .Branch(Branch),
	 .ALUControl(ALUControl),
	 .ALUSrc(ALUSrc),
	 .RegDst(RegDst),
	 .RegWrite(RegWrite)
);
  
  RegisterFile regFile(
    .clk(clk),
    .A1(Instr [25:21]),
    .A2(Instr [20:16]),
	 .A3(WriteReg),
    .WD3(Result),
    .WE3(RegWrite),
    .RD1(SrcA),
    .RD2(WriteData)
  );
  
  SignExtend signExtend(
    .In(Instr [15:0]),
    .Out(SignImm)
  );
  
  ALU alu(
    .SrcA(SrcA),
    .SrcB(SrcB),
    .ALUControl(ALUControl),
    .ALUResult(ALUResult),
    .Zero(Zero)
  );
  
  DataMemory dataMem(
	 .clk(clk),
    .A(ALUResult),
    .RD(ReadData),
	 .WD(WriteData),
    .WE(MemWrite)
  );

endmodule

module RegisterFile(
  input logic clk,
  input logic [25:21] A1,
  input logic [20:16] A2,
  input logic [20:16] A3,
  input logic [31:0] WD3,
  input logic WE3,
  output logic [31:0] RD1,
  output logic [31:0] RD2
);
  // OK
  logic [31:0] regFile[31:0];

  always_ff @(posedge clk) begin
	 if (WE3) begin
      regFile[A3] <= WD3;
    end
  end

  assign RD1 = (A1 == 0 ? 0 : regFile[A1]);
  assign RD2 = (A2 == 0 ? 0 : regFile[A2]);
	
endmodule

module ALU(
  input logic [31:0] SrcA,
  input logic [31:0] SrcB,
  input logic [2:0] ALUControl,
  output logic [31:0] ALUResult,
  output logic Zero
); // OK
  
  logic [31:0] TempResult;

  always_comb begin
    case (ALUControl)
      3'b000: TempResult = SrcA & SrcB;
      3'b001: TempResult = SrcA | SrcB;
      3'b010: TempResult = SrcA + SrcB;
		3'b011: TempResult = SrcA * SrcB;
      3'b110: TempResult = SrcA - SrcB;
      3'b111: TempResult = (SrcA < SrcB ? 1 : 0);
      default: TempResult = 0;
    endcase
  end

  assign ALUResult = TempResult;
  assign Zero = (TempResult == 0);

endmodule

module InstructionMemory(
  input logic [31:0] A,
  output logic [31:0] RD
);
  // OK
  logic [31:0] instrMem[31:0];

  initial begin
    $readmemb("instruction_memory.txt", instrMem);
  end

  assign RD = instrMem[A >> 2]; // A >> 2

endmodule

module DataMemory(
  input logic clk,
  input logic [31:0] A,
  input logic [31:0] WD,
  input logic WE,
  output logic [31:0] RD
);
  // OK
  logic [31:0] dataMem[31:0];

  initial begin
    $readmemb("data_memory.txt", dataMem);
  end

  always_ff @(posedge clk) begin
    if (WE) begin
      dataMem[A >> 2] <= WD; // A >> 2
    end
  end

  assign RD = dataMem[A >> 2]; // A >> 2

endmodule

module SignExtend(
  input logic [15:0] In,
  output logic [31:0] Out
); // OK
  assign Out = {{16{In[15]}}, In};

endmodule

module MainDecoder(
  input logic [5:0] Op,
  output logic Jump,
  output logic MemtoReg,
  output logic MemWrite,
  output logic Branch,
  output logic ALUSrc,
  output logic RegDst,
  output logic RegWrite,
  output logic [1:0] ALUOp
);

always_comb begin
        case (Op)
            6'b000000: {RegWrite, RegDst, ALUSrc, Branch, MemWrite, MemtoReg, ALUOp, Jump} = 9'b110000100; // R-type
            6'b100011: {RegWrite, RegDst, ALUSrc, Branch, MemWrite, MemtoReg, ALUOp, Jump} = 9'b101001000; // lw
            6'b101011: {RegWrite, RegDst, ALUSrc, Branch, MemWrite, MemtoReg, ALUOp, Jump} = 9'b0x101x000; // sw
            6'b000100: {RegWrite, RegDst, ALUSrc, Branch, MemWrite, MemtoReg, ALUOp, Jump} = 9'b0x010x010; // beq
            6'b000010: {RegWrite, RegDst, ALUSrc, Branch, MemWrite, MemtoReg, ALUOp, Jump} = 9'b0xxx0xxx1; // j
				6'b001000: {RegWrite, RegDst, ALUSrc, Branch, MemWrite, MemtoReg, ALUOp, Jump} = 9'b101000000; // addi
            default: {RegWrite, RegDst, ALUSrc, Branch, MemWrite, MemtoReg, ALUOp, Jump} = 9'b000000000;  // default
        endcase
end
endmodule

module ALUDecoder(
  input logic [5:0] Funct,
  input logic [1:0] ALUOp,
  output logic [2:0] ALUControl
);

always_comb begin
	case (ALUOp)
		2'b00: ALUControl <= 3'b010;
		2'b01: ALUControl <= 3'b110;
		2'b10: begin
			case (Funct)
				6'b100000: ALUControl <= 3'b010;
				6'b100001: ALUControl <= 3'b011;
				6'b100010: ALUControl <= 3'b110;
				6'b100100: ALUControl <= 3'b000;
				6'b100101: ALUControl <= 3'b001;
				6'b101010: ALUControl <= 3'b111;
				default: ALUControl <= 3'b100;
			endcase
		end
		default: ALUControl <= 3'b100;
	endcase
end

endmodule

module ControlUnit(
  output logic Jump,
  input logic [5:0] Op,
  input logic [5:0] Funct,
  output logic MemtoReg,
  output logic MemWrite,
  output logic Branch,
  output logic [2:0] ALUControl,
  output logic ALUSrc,
  output logic RegDst,
  output logic RegWrite
);
  logic [1:0] ALUOp;

  MainDecoder MD (.Op(Op), .Jump(Jump), .MemtoReg(MemtoReg), .MemWrite(MemWrite), .Branch(Branch), 
			.ALUSrc(ALUSrc), .RegDst(RegDst), .RegWrite(RegWrite), .ALUOp(ALUOp));
  
  ALUDecoder ALUD (.Funct(Funct), .ALUOp(ALUOp), .ALUControl(ALUControl));

endmodule

module top (
  input logic clk,
  input logic reset
 );
	ProcessorPrototype name (.clk(clk), .reset(reset));
 
endmodule
