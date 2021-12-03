Appendix A - Verilog Modules
`timescale 1ns / 1ps
//Module implementing the simon game
//Written by Daniel Bujalski, Thomas Oh, and Alex Grammar
module simon_game (input red, yellow, green, blue, //Color inputs
 ph1, ph2, //Clock inputs (ph1 is opposite of ph2)
 reset, //Reset pin
 output r_out, y_out, g_out, b_out,//Color outputs
 output [6:0] score, //Score output (ALSO keeps track of
 sequence length)
 output [2:0] cheat_out); //Output for testing only, outputs
 current contents of


 parameter START = 3'b000;
 parameter PRE_OUTPUT = 3'b001;
 parameter OUTPUT_SEQ = 3'b010;
 parameter INPUT_SEQ = 3'b011;
 parameter CHECK = 3'b100;
 parameter LOSE = 3'b101;
 parameter RESET = 3'b110;
 parameter WIN = 3'b111;


 //Various wires/regs
 wire input_en;
 wire [6:0] curr_pos; //Current position reference in the simon sequence memory
 wire [1:0] RNG_out; //Output of the random number generator
 wire [1:0] mem_in; //Input for writing to the sequence memory
 wire write_en; //Enable for writing to the sequence memory
 wire [1:0] mem_out; //Output from memory (contents of mem at curr_pos)
 wire [2:0] button_out; //Output from input handler (ultimately from the 'buttons')

 //Instances
 RNG_counter RNG (ph1, ph2, RNG_out); //Random number generator (RNG) instance
 //Simon sequence memory instance:
 sequence_mem simon_mem (curr_pos, mem_in, write_en, mem_out);
 //Simon input handler instance:
 input_handler simon_in (red, green, blue, yellow, input_en, button_out);
 //Simon processor instance
 simonprocessing ssm(ph1,ph2,reset,button_out,RNG_out,mem_out,write_en,r_out,y_out,g_out,
 b_out,input_en, mem_in,score,curr_pos,cheat_out);


endmodule 
//------------------------------------------------------------------------------
//Processor for simon game logic - includes controller and datapath
//------------------------------------------------------------------------------
module simonprocessing(input ph1, ph2, reset,
 input [2:0] button_out,
 input [1:0] RNG_out, mem_out,
 output write_en, read_en, r_out, y_out, g_out, b_out, input_en,
 output [1:0] mem_in,
 output [6:0] score, curr_pos,
 output [2:0] cheat_out);
 wire scoreinc,curr_posinc,resetcurr_pos; //Enables for changing score and curr_pos

 simoncontroller sc(ph1,ph2,reset,button_out,mem_out,curr_pos,score,cheat_out,scoreinc
 curr_posinc, resetcurr_pos, write_en,read_en,input_en,r_out,g_out,b_out,y_out);
 simondatapath sdp(ph1,ph2,reset,curr_posinc,scoreinc,resetcurr_pos,RNG_out,score,
 curr_pos,mem_in);

endmodule
module simoncontroller(input ph1, ph2, reset,
 input [2:0] button_out,
 input [1:0] mem_out,
 input [6:0] curr_pos, score,
 output [2:0] cheat_out,
 output scoreinc, curr_posinc, resetcurr_pos,
 output write_en, read_en, input_en,
 output r_out, g_out, b_out, y_out);
 wire [3:0] state;
 simonstatelogic ssl(ph1,ph2,reset,curr_pos,score,button_out,mem_out,state);
 simonoutputlogic sol(state,mem_out,score,curr_pos,cheat_out,scoreinc,curr_posinc,
 resetcurr_pos,write_en,read_en,input_en,r_out,g_out,b_out,y_out);

endmodule
module simonstatelogic(input ph1, ph2, reset,
 input [6:0] curr_pos, score,
 input [2:0] button_out,
 input [1:0] mem_out,
 output [3:0] state);

 reg [3:0] nextstate;
 wire [3:0] ns, state_logic;

 parameter START = 4'b0111;
 parameter PRE_OUTPUT = 4'b0001;
 parameter OUTPUT_SEQ = 4'b0010;
 parameter POST_OUT = 4'b0011;
 parameter INPUT_SEQ = 4'b0100;
 parameter CHECK = 4'b0101;
 parameter CHECK_WIN = 4'b0110;
 parameter WIN = 4'b0000;
 parameter LOSE = 4'b1000;

 
 mux2 #(4) resetmux(nextstate,START,reset,ns);
 flop #(4) statereg(ph1,ph2,ns,state_logic);
 assign state = state_logic;

 //next state logic
 always @ ( * )
 begin
 case(state)
 //Start of the machine...adds a term to the sequence end
 START: nextstate = PRE_OUTPUT;
 //Prepares for outputs by changing score/curr_pos
 PRE_OUTPUT: nextstate = OUTPUT_SEQ;
 //Determines which color is next to output
 OUTPUT_SEQ: begin
 //If these match, we hit the end of the sequence
 if (score == curr_pos) begin
 nextstate = POST_OUT; //...so we move on
 end else begin
 nextstate = OUTPUT_SEQ;
 end
 end
 //Prepares for input watching
 POST_OUT: nextstate = INPUT_SEQ;
 //Seaches for input, only transitions on button-press
 INPUT_SEQ: begin
 //Button press goes to be checked
 if (button_out[2])
 nextstate = CHECK;
 else //Otherwise keep lookin'
 nextstate = INPUT_SEQ;
 end
 //On a button press, checks against current part of sequence
 CHECK: begin
 if (button_out[1:0] == mem_out) //A match!
 nextstate = CHECK_WIN;
 //Check for a win
 else
 //A wrong button press means you lose!
 nextstate = LOSE;
 end
 //Check for the end of the sequence and/or a win (63 terms)
 CHECK_WIN: begin
 //End of the current sequence
 if (score == curr_pos)
 if (score == 7'b1000000)
 //If score reaches our max value, its a win
 nextstate = WIN;
 else
 //Otherwise, go to the beginning
 nextstate = START;
 else
 nextstate = INPUT_SEQ;
 end
 LOSE: nextstate = LOSE; //Loss...
 WIN: nextstate = WIN; //Win!
 default: nextstate = START; //Shouldn't happen
 endcase
 end
endmodule
 
module simonoutputlogic(input [3:0] state,
 input [1:0] mem_out,
 input [6:0] score, curr_pos,
 output reg [2:0] cheat_out,
 output reg scoreinc, curr_posinc, resetcurr_pos,
 output reg write_en, read_en, input_en,
 output reg r_out, g_out, b_out, y_out);

 parameter START = 4'b0111;
 parameter PRE_OUTPUT = 4'b0001;
 parameter OUTPUT_SEQ = 4'b0010;
 parameter POST_OUT = 4'b0011;
 parameter INPUT_SEQ = 4'b0100;
 parameter CHECK = 4'b0101;
 parameter CHECK_WIN = 4'b0110;
 parameter WIN = 4'b0000;
 parameter LOSE = 4'b1000;


 always @ ( * )
 begin
 write_en = 0; //Enable memory writing
 read_en = 0; //Enable memory reading (for output)
 scoreinc = 0; //increment score
 curr_posinc = 0; //increment curr_pos
 input_en = 0; //Enable input tracking
 resetcurr_pos = 0; //Reset curr_pos
 r_out = 0; //Color outputs
 g_out = 0;
 b_out = 0;
 y_out = 0;
 cheat_out = 3'b000; //Output for testbench
 case(state)
 START: //Start of the machine...adds a term to the sequence end
 begin
 //Write new term to memory
 write_en = 1;
 end
 PRE_OUTPUT: //Prepares for outputs by changing score/curr_pos
 begin
 scoreinc = 1; //Enable score increment
 resetcurr_pos = 1; //Enable curr_pos reset
 end
 OUTPUT_SEQ: //Increment curr_pos each time through
 begin
 //Only does this if curr_pos is less than score
 if (curr_pos != score) begin
 curr_posinc = 1;
 read_en = 1;
 case (mem_out)
 2'b00: r_out = 1;
 2'b01: b_out = 1;
 2'b10: g_out = 1;
 2'b11: y_out = 1;
 default: begin end//nothing
 endcase
 end
 end
 POST_OUT: //Post-outputting setup for input handling
 begin
 resetcurr_pos = 1; //Enable curr_pos reset
 end 
 INPUT_SEQ: //Looks for an input
 begin
 read_en = 1; //Read from memory for cheat_out
 input_en = 1; //Input handling enabled
 //Send out cheat_out for testbench
 cheat_out = {1'b0,mem_out} + 3'b100;
 end
 CHECK: //Upon a button press, perform actions related to it
 begin
 read_en = 1;
 input_en = 1; //Keep input enable high for check
 curr_posinc = 1; //Increment curr_pos in any case
 end
 CHECK_WIN: //Checks to see if player has won
 begin //Currently doesn't need to do anything
 end
 LOSE:
 begin //Player loses, light half the outputs
 r_out = 1'b1;
 b_out = 1'b0;
 g_out = 1'b1;
 y_out = 1'b0;
 end
 WIN:
 begin //Player wins, flash all outputs at once
 r_out = 1'b1;
 b_out = 1'b1;
 g_out = 1'b1;
 y_out = 1'b1;
 end
 default: begin //Shouldn't be reached
 end
 endcase
 end
endmodule
module simondatapath(input ph1, ph2, reset,
 input curr_posinc, scoreinc, resetcurr_pos,
 input [1:0] RNG_out,
 output [6:0] score, curr_pos,
 output [1:0] mem_in);

 flopenr #(7) curr_pos_flop(ph1, ph2, (reset || resetcurr_pos), curr_posinc,
 curr_pos+1'b1, curr_pos);
 flopenr #(7) score_flop(ph1, ph2, reset, scoreinc, score+1'b1, score);
 flop #(2) add_term_flop(ph1,ph2,RNG_out,mem_in);

endmodule 
//------------------------------------------------------------------------------
//RNG_counter to generate random numbers for sequence
//------------------------------------------------------------------------------
module RNG_counter( input ph1, ph2,
 output [1:0] count_out);

 reg [1:0] count = 0;

 flop #(2) countflop(ph1,ph2,count,count_out);

 always @ ( * )
 count <= count_out + 1;
endmodule
//------------------------------------------------------------------------------
//Sequence memory for storing the stages (note: does not have a read enable here
//as it was introduced manually in schematic)
//------------------------------------------------------------------------------
module sequence_mem(input [6:0] curr_pos,
 input [1:0] RNG_in,
 input write_en,
 output [1:0] color_out);
 reg [1:0] seq_mem[2**6:0];

 //Write contents of RNG_in to memory location curr_pos
 //when write_en is set high (on edge)
 always @ (posedge write_en)
 seq_mem[curr_pos] <= RNG_in;

 //Module outputs the contents of memory at curr_pos
 assign color_out = seq_mem[curr_pos];
endmodule
//------------------------------------------------------------------------------
//Input handler module for the simon game which avoids multiple inputs in series
//------------------------------------------------------------------------------
module input_handler(
 input r, g, b, y,
 input en,
 output [2:0] color_out
 );
 wire [2:0] mid;

 mux2 #(3) endmux(3'b000, mid, en, color_out);
 mux16 #(3) decodermux(3'b100,3'b101,3'b110,3'b111,3'b000,{y,g,b,r},mid);

endmodule
//Special mux for input (each bit of input is one of the lights)
module mux16 #(parameter WIDTH = 8)
 (input [WIDTH-1:0] d0, d1, d2, d3, d4,
 input [3:0] s,
 output reg [WIDTH-1:0] y); 
 always @ ( * )
 case(s)
 4'b0001: y = d0;
 4'b0010: y = d1;
 4'b0100: y = d2;
 4'b1000: y = d3;
 default: y = d4;
 endcase
endmodule
//Other useful blocks - flops, muxes, etc.
module flop #(parameter WIDTH = 8)
 (input ph1, ph2,
 input [WIDTH-1:0] d,
 output [WIDTH-1:0] q);
 wire [WIDTH-1:0] mid;
 latch #(WIDTH) master(ph2, d, mid);
 latch #(WIDTH) slave(ph1, mid, q);
endmodule
module flopen #(parameter WIDTH = 8)
 (input ph1, ph2, en,
 input [WIDTH-1:0] d,
 output [WIDTH-1:0] q);
 wire [WIDTH-1:0] d2;
 mux2 #(WIDTH) enmux(q, d, en, d2);
 flop #(WIDTH) f(ph1, ph2, d2, q);
endmodule
module flopenr #(parameter WIDTH = 8)
 (input ph1, ph2, reset, en,
 input [WIDTH-1:0] d,
 output [WIDTH-1:0] q);

 wire [WIDTH-1:0] resetval, d2, mid;
 // assign resetval = 0;
// mux3 #(WIDTH) enrmux(q, d, resetval, {reset, en}, d2);
 mux2 #(WIDTH) enmux(q, d, en, mid);
//Used to be WIDTH'b0, but synthesis would not take it, so is at the moment a hard value
// mux2 #(WIDTH) resetmux(mid,{WIDTH{1'b0}},reset,d2);
// flop #(WIDTH) f(ph1, ph2, d2, q);
 flopr #(WIDTH) f(ph1, ph2, reset, mid, q);
endmodule
module flopr #(parameter WIDTH = 8)
 (input ph1, ph2, reset,
 input [WIDTH-1:0] d,
 output [WIDTH-1:0] q);
 wire [WIDTH-1:0] mid;
 latchr #(WIDTH) master(ph2, reset, d, mid); 
 latch #(WIDTH) slave(ph1, mid, q);
endmodule
module latchr #(parameter WIDTH = 8)
 (input ph, reset,
 input [WIDTH-1:0] d,
 output reg [WIDTH-1:0] q);
 always @ ( * )
 if (ph)
 if (reset) q <= {WIDTH{1'b0}};
 else q <= d;
endmodule
module latch #(parameter WIDTH = 8)
 (input ph,
 input [WIDTH-1:0] d,
 output reg [WIDTH-1:0] q);
 always @ ( * )
 if (ph) q <= d;
endmodule
module mux2 #(parameter WIDTH = 8)
 (input [WIDTH-1:0] d0, d1,
 input s,
 output [WIDTH-1:0] y);
 assign y = s ? d1 : d0;
endmodule
module mux3 #(parameter WIDTH = 8)
 (input [WIDTH-1:0] d0, d1, d2,
 input [1:0] s,
 output reg [WIDTH-1:0] y);
 always @ ( * )
 casez (s)
 2'b00: y = d0;
 2'b01: y = d1;
 2'b1?: y = d2;
 endcase
endmodule 
Appendix B - Testbe