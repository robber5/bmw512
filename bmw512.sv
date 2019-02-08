`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: Holodnak Electronics
// Engineer: James Holodnak
//
//  http://github.com/holodnak/bmw512
// 
// Create Date: 02/05/2019 06:54:12 PM
// Design Name: BMW512 Hash Core
// Module Name: bmw512
// Project Name: vcu1525_bmw512
// Target Devices: ZCU104, KCU1500, VCU1525
//
// Description: BMW512 pipelined hash core.  Takes 164 cycles to output valid hash.
//     Runs at 750mhz+ speeds on ZCU104 board (single core build).
//////////////////////////////////////////////////////////////////////////////////

function [63:0] shr;
    input [63:0] n;
    input [31:0] x;
    begin
        shr = (n) >> (x);
    end
endfunction

function [63:0] shl;
    input [63:0] n;
    input [31:0] x;
    begin
        shl = (n) << (x);
    end
endfunction

function [63:0] rotl64;
    input [63:0] n;
    input [31:0] x;
    begin
        rotl64 = ((n) << (x)) | ((n) >> (64 - (x)));
    end
endfunction

function [63:0] s64_0;
    input [63:0] x;
    begin
        s64_0 = (shr((x), 1) ^ shl((x), 3) ^ rotl64((x),  4) ^ rotl64((x), 37));
    end
endfunction

function [63:0] s64_1;
    input [63:0] x;
    begin
        s64_1 = (shr((x), 1) ^ shl((x), 2) ^ rotl64((x), 13) ^ rotl64((x), 43));
    end
endfunction

function [63:0] s64_2;
    input [63:0] x;
    begin
        s64_2 = (shr((x), 2) ^ shl((x), 1) ^ rotl64((x), 19) ^ rotl64((x), 53));
    end
endfunction

function [63:0] s64_3;
    input [63:0] x;
    begin
        s64_3 = (shr((x), 2) ^ shl((x), 2) ^ rotl64((x), 28) ^ rotl64((x), 59));
    end
endfunction

function [63:0] s64_4;
    input [63:0] x;
    begin
        s64_4 = (shr((x), 1) ^ (x));
    end
endfunction

function [63:0] s64_5;
    input [63:0] x;
    begin
        s64_5 = (shr((x), 2) ^ (x));
    end
endfunction

function [63:0] r64_01;
    input [63:0] x;
    begin
        r64_01 = rotl64((x),  5);
    end
endfunction

function [63:0] r64_02;
    input [63:0] x;
    begin
        r64_02 = rotl64((x),  11);
    end
endfunction

function [63:0] r64_03;
    input [63:0] x;
    begin
        r64_03 = rotl64((x),  27);
    end
endfunction

function [63:0] r64_04;
    input [63:0] x;
    begin
        r64_04 = rotl64((x),  32);
    end
endfunction

function [63:0] r64_05;
    input [63:0] x;
    begin
        r64_05 = rotl64((x),  37);
    end
endfunction

function [63:0] r64_06;
    input [63:0] x;
    begin
        r64_06 = rotl64((x),  43);
    end
endfunction

function [63:0] r64_07;
    input [63:0] x;
    begin
        r64_07 = rotl64((x),  53);
    end
endfunction

`define IV(n)  iv[ (((15-(n))*64)+63) : ((15-(n))*64) ]
`define IDX(n) (((n)*64)+63) : ((n)*64)

module bmw512_f0 #( parameter [63:0] H[0:15] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ) (
    input           clk,
    input  [63:0]   M[0:15],
    output  [63:0]  M_out[0:15],
    output  [63:0]  Q_out[0:15]
);

    reg [63:0] M_1[0:15], M_2[0:15], M_3[0:15], M_4[0:15], M_5[0:15], M_6[0:15];
    reg [63:0] W[0:15], Q[0:15];

    reg [63:0] MH_1[0:15];
    reg [63:0] MH_2[0:15];
    reg [63:0] MH_3[0:15];
    reg [63:0] MH_4[0:15];
    
    assign Q_out = Q;
    assign M_out = M_6;

    //======= f0 ================    

    always @ (posedge clk)
    begin
        $display("M64[0..3]= %x %x %x %x", M[ 0],M[ 1],M[ 2],M[ 3]);
        $display("M64[4..7]= %x %x %x %x", M[ 4],M[ 5],M[ 6],M[ 7]);
        $display("M64[8..B]= %x %x %x %x", M[ 8],M[ 9],M[10],M[11]);
        $display("M64[C..F]= %x %x %x %x", M[12],M[13],M[14],M[15]);        
        M_1 <= M;
        M_2 <= M_1;
        M_3 <= M_2;
        M_4 <= M_3;
        M_5 <= M_4;
        M_6 <= M_5;
    end
    
    reg [63:0] W0[0:15];
    reg [63:0] W1[0:15];
    reg [63:0] W2[0:15];

    always @ (posedge clk)
    begin
    
        MH_2 <= MH_1;
        MH_3 <= MH_2;
        MH_4 <= MH_3;

        MH_1[0] <= M[0] ^ H[0];
        MH_1[1] <= M[1] ^ H[1];
        MH_1[2] <= M[2] ^ H[2];
        MH_1[3] <= M[3] ^ H[3];
        MH_1[4] <= M[4] ^ H[4];
        MH_1[5] <= M[5] ^ H[5];
        MH_1[6] <= M[6] ^ H[6];
        MH_1[7] <= M[7] ^ H[7];
        MH_1[8] <= M[8] ^ H[8];
        MH_1[9] <= M[9] ^ H[9];
        MH_1[10] <= M[10] ^ H[10];
        MH_1[11] <= M[11] ^ H[11];
        MH_1[12] <= M[12] ^ H[12];
        MH_1[13] <= M[13] ^ H[13];
        MH_1[14] <= M[14] ^ H[14];
        MH_1[15] <= M[15] ^ H[15];
        
        W [ 0] <= W2[ 0] + MH_4[14];
        W [ 1] <= W2[ 1] - MH_4[15];
        W [ 2] <= W2[ 2] + MH_4[15];
        W [ 3] <= W2[ 3] + MH_4[13];
        W [ 4] <= W2[ 4] - MH_4[14];
        W [ 5] <= W2[ 5] + MH_4[15];
        W [ 6] <= W2[ 6] + MH_4[13];
        W [ 7] <= W2[ 7] - MH_4[14];
        W [ 8] <= W2[ 8] - MH_4[15];
        W [ 9] <= W2[ 9] + MH_4[14];
        W [10] <= W2[10] + MH_4[15];
        W [11] <= W2[11] + MH_4[ 9];
        W [12] <= W2[12] + MH_4[10];
        W [13] <= W2[13] + MH_4[11];
        W [14] <= W2[14] - MH_4[12];
        W [15] <= W2[15] + MH_4[13];

        W2[ 0] <= W1[ 0] + MH_3[13];
        W2[ 1] <= W1[ 1] + MH_3[14];
        W2[ 2] <= W1[ 2] - MH_3[12];
        W2[ 3] <= W1[ 3] - MH_3[10];
        W2[ 4] <= W1[ 4] - MH_3[11];
        W2[ 5] <= W1[ 5] - MH_3[12];
        W2[ 6] <= W1[ 6] - MH_3[11];
        W2[ 7] <= W1[ 7] - MH_3[12];
        W2[ 8] <= W1[ 8] + MH_3[13];
        W2[ 9] <= W1[ 9] - MH_3[ 7];
        W2[10] <= W1[10] - MH_3[ 7];
        W2[11] <= W1[11] - MH_3[ 5];
        W2[12] <= W1[12] - MH_3[ 9];
        W2[13] <= W1[13] + MH_3[10];
        W2[14] <= W1[14] - MH_3[11];
        W2[15] <= W1[15] - MH_3[ 9];

        W1[ 0] <= W0[ 0] + MH_2[10];
        W1[ 1] <= W0[ 1] + MH_2[11];
        W1[ 2] <= W0[ 2] + MH_2[ 9];
        W1[ 3] <= W0[ 3] + MH_2[ 8];
        W1[ 4] <= W0[ 4] + MH_2[ 9];
        W1[ 5] <= W0[ 5] + MH_2[10];
        W1[ 6] <= W0[ 6] - MH_2[ 3];
        W1[ 7] <= W0[ 7] - MH_2[ 5];
        W1[ 8] <= W0[ 8] - MH_2[ 6];
        W1[ 9] <= W0[ 9] + MH_2[ 6];
        W1[10] <= W0[10] - MH_2[ 4];
        W1[11] <= W0[11] - MH_2[ 2];
        W1[12] <= W0[12] - MH_2[ 6];
        W1[13] <= W0[13] + MH_2[ 7];
        W1[14] <= W0[14] + MH_2[ 8];
        W1[15] <= W0[15] - MH_2[ 6];

        W0[ 0] <= MH_1[ 5] - MH_1[7];
        W0[ 1] <= MH_1[ 6] - MH_1[8];
        W0[ 2] <= MH_1[ 0] + MH_1[7];
        W0[ 3] <= MH_1[ 0] - MH_1[1];
        W0[ 4] <= MH_1[ 1] + MH_1[2];
        W0[ 5] <= MH_1[ 3] - MH_1[2];
        W0[ 6] <= MH_1[ 4] - MH_1[0];
        W0[ 7] <= MH_1[ 1] - MH_1[4];
        W0[ 8] <= MH_1[ 2] - MH_1[5];
        W0[ 9] <= MH_1[ 0] - MH_1[3];
        W0[10] <= MH_1[ 8] - MH_1[1];
        W0[11] <= MH_1[ 8] - MH_1[0];
        W0[12] <= MH_1[ 1] + MH_1[3];
        W0[13] <= MH_1[ 2] + MH_1[4];
        W0[14] <= MH_1[ 3] - MH_1[5];
        W0[15] <= MH_1[12] - MH_1[4];

        $display("W[0..3]= %x %x %x %x", W[ 0],W[ 1],W[ 2],W[ 3]);
        $display("W[4..7]= %x %x %x %x", W[ 4],W[ 5],W[ 6],W[ 7]);
        $display("W[8..B]= %x %x %x %x", W[ 8],W[ 9],W[10],W[11]);
        $display("W[C..F]= %x %x %x %x", W[12],W[13],W[14],W[15]);        
    end

    always @ (posedge clk)
    begin
        Q[0] <= s64_0(W[0]) + H[1];
        Q[1] <= s64_1(W[1]) + H[2];
        Q[2] <= s64_2(W[2]) + H[3];
        Q[3] <= s64_3(W[3]) + H[4];
        Q[4] <= s64_4(W[4]) + H[5];
        Q[5] <= s64_0(W[5]) + H[6];
        Q[6] <= s64_1(W[6]) + H[7];
        Q[7] <= s64_2(W[7]) + H[8];
        Q[8] <= s64_3(W[8]) + H[9];
        Q[9] <= s64_4(W[9]) + H[10];
        Q[10] <= s64_0(W[10]) + H[11];
        Q[11] <= s64_1(W[11]) + H[12];
        Q[12] <= s64_2(W[12]) + H[13];
        Q[13] <= s64_3(W[13]) + H[14];
        Q[14] <= s64_4(W[14]) + H[15];
        Q[15] <= s64_0(W[15]) + H[0];
        $display("Q[0..3]= %x %x %x %x", Q[ 0],Q[ 1],Q[ 2],Q[ 3]);
        $display("Q[4..7]= %x %x %x %x", Q[ 4],Q[ 5],Q[ 6],Q[ 7]);
        $display("Q[8..B]= %x %x %x %x", Q[ 8],Q[ 9],Q[10],Q[11]);
        $display("Q[C..F]= %x %x %x %x", Q[12],Q[13],Q[14],Q[15]);
    end

endmodule

module expand64_1 #( parameter offset = 32'd0, parameter [63:0] H[0:15] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} )(
    input clk,
    input [63:0] M[0:15],
    input [63:0] Q[0:31],
    output [63:0] M_out[0:15],
    output [63:0] Q_out[0:31]
);

    reg [63:0] Q_1[0:31];
    reg [63:0] Q_2[0:31];
    reg [63:0] Q_3[0:31];
    reg [63:0] Q_4[0:31];
    reg [63:0] Q_5[0:31];

    reg [63:0] M_1[0:15];
    reg [63:0] M_2[0:15];
    reg [63:0] M_3[0:15];
    reg [63:0] M_4[0:15];
    reg [63:0] M_5[0:15];
    
    assign M_out = M_5;
    assign Q_out = Q_5;
        
    always @ (posedge clk)
    begin
        M_1 <= M;
        M_2 <= M_1;
        M_3 <= M_2;
        M_4 <= M_3;
        M_5 <= M_4;
    end
       
    reg [63:0] T0[0:15];
    reg [63:0] T1[0:5];
    reg [63:0] T2[0:1];
    reg [63:0] T3;
    reg [63:0] V0[0:3];
    reg [63:0] V1, V2, V3;
    
    always @ (posedge clk)
    begin
        Q_1 <= Q;
        Q_2 <= Q_1;
        Q_3 <= Q_2;
        Q_4 <= Q_3;
        Q_5 <= Q_4;

        T0[ 0] <= s64_1(Q[(offset + 16) - 16]);
        T0[ 1] <= s64_2(Q[(offset + 16) - 15]);
        T0[ 2] <= s64_1(Q[(offset + 16) - 12]);
        T0[ 3] <= s64_2(Q[(offset + 16) - 11]);
        T0[ 4] <= s64_1(Q[(offset + 16) -  8]);
        T0[ 5] <= s64_2(Q[(offset + 16) -  7]);
        T0[ 6] <= s64_1(Q[(offset + 16) -  4]);
        T0[ 7] <= s64_2(Q[(offset + 16) -  3]);
        T0[ 8] <= s64_3(Q[(offset + 16) - 14]);
        T0[ 9] <= s64_0(Q[(offset + 16) - 13]);
        T0[10] <= s64_3(Q[(offset + 16) - 10]);
        T0[11] <= s64_0(Q[(offset + 16) -  9]);
        T0[12] <= s64_3(Q[(offset + 16) -  6]);
        T0[13] <= s64_0(Q[(offset + 16) -  5]);
        T0[14] <= s64_3(Q[(offset + 16) -  2]);
        T0[15] <= s64_0(Q[(offset + 16) -  1]);

        T1[0] <= T0[ 0] + T0[ 6] + T0[12];
        T1[1] <= T0[ 1] + T0[ 7] + T0[13];
        T1[2] <= T0[ 2] + T0[ 8] + T0[14];
        T1[3] <= T0[ 3] + T0[ 9] + T0[15];
        T1[4] <= T0[ 4] + T0[10];
        T1[5] <= T0[ 5] + T0[11];
        
        T2[0] <= T1[0] + T1[1] + T1[2];
        T2[1] <= T1[3] + T1[4] + T1[5];

        T3 <= T2[0] + T2[1];
        
        V0[0] <= rotl64(M[((offset + 16) - 16) % 16], (((offset + 16) - 16) % 16) + 1) + ((offset + 16)*(64'h0555555555555555));
        V0[1] <= rotl64(M[((offset + 16) - 13) % 16], (((offset + 16) - 13) % 16) + 1);
        V0[2] <= rotl64(M[((offset + 16) - 6) % 16], (((offset + 16) - 6) % 16) + 1);
        
        V1 <= V0[0] + V0[1] - V0[2];
        
        V2 <= V1 ^ H[((offset + 16) - 16 + 7) % 16];
        
        V3 <= V2;

        Q_5[(offset + 16)] <= T3 + V3;
    end
    
endmodule

module expand64_2 #( parameter offset = 32'd0, parameter [63:0] H[0:15] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0})(
    input clk,
    input [63:0] M[0:15],
    input [63:0] Q[0:31],
    output [63:0] M_out[0:15],
    output [63:0] Q_out[0:31]
);

    reg [63:0] Q_1[0:31];
    reg [63:0] Q_2[0:31];
    reg [63:0] Q_3[0:31];
    reg [63:0] Q_4[0:31];
    
    reg [63:0] M_1[0:15];
    reg [63:0] M_2[0:15];
    reg [63:0] M_3[0:15];
    reg [63:0] M_4[0:15];
    
    assign M_out = M_4;
    assign Q_out = Q_4;
        
    always @ (posedge clk)
    begin
        M_1 <= M;
        M_2 <= M_1;
        M_3 <= M_2;
        M_4 <= M_3;
    end

    reg [63:0] T0[0:7];
    reg [63:0] T1[0:2];
    reg [63:0] T2;
    reg [63:0] V0[0:3];
    reg [63:0] V1;
    reg [63:0] V2;
    
    always @ (posedge clk)
    begin
        Q_1 <= Q;
        Q_2 <= Q_1;
        Q_3 <= Q_2;
        Q_4 <= Q_3;

        T0[0] <= Q[(offset + 16) - 16] + r64_01(Q[(offset + 16) - 15]);
        T0[1] <= Q[(offset + 16) - 14] + r64_02(Q[(offset + 16) - 13]);
        T0[2] <= Q[(offset + 16) - 12] + r64_03(Q[(offset + 16) - 11]);
        T0[3] <= Q[(offset + 16) - 10] + r64_04(Q[(offset + 16) - 9]);
        T0[4] <= Q[(offset + 16) -  8] + r64_05(Q[(offset + 16) -  7]);
        T0[5] <= Q[(offset + 16) - 6] + r64_06(Q[(offset + 16) - 5]);
        T0[6] <= Q[(offset + 16) -  4] + r64_07(Q[(offset + 16) -  3]);
        T0[7] <= s64_4(Q[(offset + 16) - 2]) + s64_5(Q[(offset + 16) - 1]);
        V0[0] <= rotl64(M[((offset + 16) - 16) % 16], (((offset + 16) - 16) % 16) + 1) + ((offset + 16)*(64'h0555555555555555));
        V0[1] <= rotl64(M[((offset + 16) - 13) % 16], (((offset + 16) - 13) % 16) + 1);
        V0[2] <= rotl64(M[((offset + 16) - 6) % 16], (((offset + 16) - 6) % 16) + 1);
        
        T1[0] <= T0[0] + T0[3] + T0[6];
        T1[1] <= T0[1] + T0[4] + T0[7];
        T1[2] <= T0[2] + T0[5];
        V1 <= V0[0] + V0[1] - V0[2];
        
        T2 <= T1[0] + T1[1] + T1[2];
        V2 <= V1 ^ H[((offset + 16) - 16 + 7) % 16];

        Q_4[(offset + 16)] <= T2 + V2;
    end
    
endmodule

module bmw512_f1 #( parameter [63:0] H[0:15] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ) (
    input           clk,
    input  [63:0]   M[0:15],
    input  [63:0]   Q[0:15],    
    output  [63:0]  M_out[0:15],
    output  [63:0]  Q_out[0:31],
    output  [63:0]  XL64_out,
    output  [63:0]  XH64_out
);

    wire [63:0] R[0:15], Q_0[0:31];
    wire [63:0] M_1[0:15], M_5[0:15], M_9[0:15], M_D[0:15];
    wire [63:0] M_2[0:15], M_6[0:15], M_A[0:15], M_E[0:15];
    wire [63:0] M_3[0:15], M_7[0:15], M_B[0:15], M_F[0:15];
    wire [63:0] M_4[0:15], M_8[0:15], M_C[0:15], M_G[0:15];
    wire [63:0] Q_1[0:31], Q_5[0:31], Q_9[0:31], Q_D[0:31];
    wire [63:0] Q_2[0:31], Q_6[0:31], Q_A[0:31], Q_E[0:31];
    wire [63:0] Q_3[0:31], Q_7[0:31], Q_B[0:31], Q_F[0:31];
    wire [63:0] Q_4[0:31], Q_8[0:31], Q_C[0:31], Q_G[0:31];
    
    reg [63:0] M_H[0:15], M_I[0:15], M_J[0:15];
    reg [63:0] Q_H[0:31], Q_I[0:31], Q_J[0:31];
    reg [63:0] M_K[0:15], M_L[0:15];
    reg [63:0] Q_K[0:31], Q_L[0:31];

    assign M_out = M_J;
    assign Q_out = Q_J;
    
    genvar i;
    generate
        for(i=0;i<16;i=i+1) begin: Q_assign
            assign Q_0[i]    = Q[i];
            assign Q_0[i+16] = 64'd0;
        end
    endgenerate
    
    always @ (posedge clk)
    begin
        M_H <= M_G;
        M_I <= M_H;
        M_J <= M_I;
        Q_H <= Q_G;
        Q_I <= Q_H;
        Q_J <= Q_I;
    end

    reg [63:0] XL64, XH64;
    
    assign XL64_out = XL64;
    assign XH64_out = XH64;
    
    reg [63:0] T[0:3];
    
    always @ (posedge clk)
    begin
        T[0] <= Q_H[16]^Q_H[17]^Q_H[18]^Q_H[19];
        T[1] <= Q_H[20]^Q_H[21]^Q_H[22]^Q_H[23];

        T[2] <= Q_H[24]^Q_H[25]^Q_H[26]^Q_H[27];
        T[3] <= Q_H[28]^Q_H[29]^Q_H[30]^Q_H[31];
        
        XL64 <= T[0] ^ T[1];
        XH64 <= T[0] ^ T[1] ^ T[2] ^ T[3];

        $display("XL64, XH64= %x %x", XL64, XH64);
    end

    wire [63:0] P[0:15];
    
    generate
        for(i=0;i<16;i=i+1) begin: P_assign
            assign P[i] = Q_G[i+16];
        end
    endgenerate

    always @ (posedge clk)
    begin
        $display("P[0..3]= %x %x %x %x", P[ 0],P[ 1],P[ 2],P[ 3]);
        $display("P[4..7]= %x %x %x %x", P[ 4],P[ 5],P[ 6],P[ 7]);
        $display("P[8..B]= %x %x %x %x", P[ 8],P[ 9],P[10],P[11]);
        $display("P[C..F]= %x %x %x %x", P[12],P[13],P[14],P[15]);
    end
    
    expand64_1 #( 0, H) e0  (   .clk(clk),  .M(  M),  .Q(Q_0),  .M_out(M_1),  .Q_out(Q_1));
    expand64_1 #( 1, H) e1  (   .clk(clk),  .M(M_1),  .Q(Q_1),  .M_out(M_2),  .Q_out(Q_2));
    expand64_2 #( 2, H) e2  (   .clk(clk),  .M(M_2),  .Q(Q_2),  .M_out(M_3),  .Q_out(Q_3));
    expand64_2 #( 3, H) e3  (   .clk(clk),  .M(M_3),  .Q(Q_3),  .M_out(M_4),  .Q_out(Q_4));
    expand64_2 #( 4, H) e4  (   .clk(clk),  .M(M_4),  .Q(Q_4),  .M_out(M_5),  .Q_out(Q_5));
    expand64_2 #( 5, H) e5  (   .clk(clk),  .M(M_5),  .Q(Q_5),  .M_out(M_6),  .Q_out(Q_6));
    expand64_2 #( 6, H) e6  (   .clk(clk),  .M(M_6),  .Q(Q_6),  .M_out(M_7),  .Q_out(Q_7));
    expand64_2 #( 7, H) e7  (   .clk(clk),  .M(M_7),  .Q(Q_7),  .M_out(M_8),  .Q_out(Q_8));
    expand64_2 #( 8, H) e8  (   .clk(clk),  .M(M_8),  .Q(Q_8),  .M_out(M_9),  .Q_out(Q_9));
    expand64_2 #( 9, H) e9  (   .clk(clk),  .M(M_9),  .Q(Q_9),  .M_out(M_A),  .Q_out(Q_A));
    expand64_2 #(10, H) eA  (   .clk(clk),  .M(M_A),  .Q(Q_A),  .M_out(M_B),  .Q_out(Q_B));
    expand64_2 #(11, H) eB  (   .clk(clk),  .M(M_B),  .Q(Q_B),  .M_out(M_C),  .Q_out(Q_C));
    expand64_2 #(12, H) eC  (   .clk(clk),  .M(M_C),  .Q(Q_C),  .M_out(M_D),  .Q_out(Q_D));
    expand64_2 #(13, H) eD  (   .clk(clk),  .M(M_D),  .Q(Q_D),  .M_out(M_E),  .Q_out(Q_E));
    expand64_2 #(14, H) eE  (   .clk(clk),  .M(M_E),  .Q(Q_E),  .M_out(M_F),  .Q_out(Q_F));
    expand64_2 #(15, H) eF  (   .clk(clk),  .M(M_F),  .Q(Q_F),  .M_out(M_G),  .Q_out(Q_G));

endmodule

module bmw512_f2 (
    input           clk,
    input  [63:0]   M[0:15],
    input  [63:0]   Q[0:31],
    input  [63:0]   XL64,
    input  [63:0]   XH64,    
    output  [63:0]  H_out[0:15]
);

    reg [63:0] Y[0:15];

    reg [63:0] T[0:15];
    reg [63:0] V[0:15];
    reg [63:0] S[0:7];
    reg [63:0] R[0:7];
    
    assign H_out = Y;

    always @ (posedge clk)
    begin
        T[ 0] <= (shl(XH64,  5) ^ shr(Q[16], 5) ^ M[0]);
        T[ 1] <= (XL64          ^ Q[24]         ^ Q[0]);
        T[ 2] <= (shr(XH64,  7) ^ shl(Q[17], 8) ^ M[1]);
        T[ 3] <= (XL64          ^ Q[25]         ^ Q[1]);
        T[ 4] <= (shr(XH64,  5) ^ shl(Q[18], 5) ^ M[2]);
        T[ 5] <= (XL64          ^ Q[26]         ^ Q[2]);
        T[ 6] <= (shr(XH64,  1) ^ shl(Q[19], 5) ^ M[3]);
        T[ 7] <= (XL64          ^ Q[27]         ^ Q[3]);
        T[ 8] <= (shr(XH64,  3) ^ Q[20]         ^ M[4]);
        T[ 9] <= (XL64          ^ Q[28]         ^ Q[4]);
        T[10] <= (shl(XH64,  6) ^ shr(Q[21], 6) ^ M[5]);
        T[11] <= (XL64          ^ Q[29]         ^ Q[5]);
        T[12] <= (shr(XH64,  4) ^ shl(Q[22], 6) ^ M[6]);
        T[13] <= (XL64          ^ Q[30]         ^ Q[6]);
        T[14] <= (shr(XH64, 11) ^ shl(Q[23], 2) ^ M[7]);
        T[15] <= (XL64          ^ Q[31]         ^ Q[7]);

        V[ 0] <= (XH64     ^     Q[24] ^ M[ 8]);
        V[ 1] <= (shl(XL64, 8) ^ Q[23] ^ Q[ 8]);
        V[ 2] <= (XH64     ^     Q[25] ^ M[ 9]);
        V[ 3] <= (shr(XL64, 6) ^ Q[16] ^ Q[ 9]);
        V[ 4] <= (XH64     ^     Q[26] ^ M[10]);
        V[ 5] <= (shl(XL64, 6) ^ Q[17] ^ Q[10]);
        V[ 6] <= (XH64     ^     Q[27] ^ M[11]);
        V[ 7] <= (shl(XL64, 4) ^ Q[18] ^ Q[11]);
        V[ 8] <= (XH64     ^     Q[28] ^ M[12]);
        V[ 9] <= (shr(XL64, 3) ^ Q[19] ^ Q[12]);
        V[10] <= (XH64     ^     Q[29] ^ M[13]);
        V[11] <= (shr(XL64, 4) ^ Q[20] ^ Q[13]);
        V[12] <= (XH64     ^     Q[30] ^ M[14]);
        V[13] <= (shr(XL64, 7) ^ Q[21] ^ Q[14]);
        V[14] <= (XH64     ^     Q[31] ^ M[15]);
        V[15] <= (shr(XL64, 2) ^ Q[22] ^ Q[15]);
                
        S[0] <= T[ 0] + T[ 1];
        S[1] <= T[ 2] + T[ 3];
        S[2] <= T[ 4] + T[ 5];
        S[3] <= T[ 6] + T[ 7];
        S[4] <= T[ 8] + T[ 9];
        S[5] <= T[10] + T[11];
        S[6] <= T[12] + T[13];
        S[7] <= T[14] + T[15];

        R[0] <= V[ 0] + V[ 1];
        R[1] <= V[ 2] + V[ 3];
        R[2] <= V[ 4] + V[ 5];
        R[3] <= V[ 6] + V[ 7];
        R[4] <= V[ 8] + V[ 9];
        R[5] <= V[10] + V[11];
        R[6] <= V[12] + V[13];
        R[7] <= V[14] + V[15];

        Y[ 0] <= S[0];
        Y[ 1] <= S[1];
        Y[ 2] <= S[2];
        Y[ 3] <= S[3];
        Y[ 4] <= S[4];
        Y[ 5] <= S[5];
        Y[ 6] <= S[6];
        Y[ 7] <= S[7];
        Y[ 8] <= rotl64(S[4],  9) + R[0];
        Y[ 9] <= rotl64(S[5], 10) + R[1];
        Y[10] <= rotl64(S[6], 11) + R[2];
        Y[11] <= rotl64(S[7], 12) + R[3];
        Y[12] <= rotl64(S[0], 13) + R[4];
        Y[13] <= rotl64(S[1], 14) + R[5];
        Y[14] <= rotl64(S[2], 15) + R[6];
        Y[15] <= rotl64(S[3], 16) + R[7];

        $display("Y[0..3]= %x %x %x %x", Y[ 0],Y[ 1],Y[ 2],Y[ 3]);
        $display("Y[4..7]= %x %x %x %x", Y[ 4],Y[ 5],Y[ 6],Y[ 7]);
        $display("Y[8..B]= %x %x %x %x", Y[ 8],Y[ 9],Y[10],Y[11]);
        $display("Y[C..F]= %x %x %x %x", Y[12],Y[13],Y[14],Y[15]);
    end

endmodule

module bmw512_compress #( parameter [63:0]H[0:15] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} )(
    input clk,
    input  [1023:0] data,    //data in
    output [1023:0] hash     //hash out
);

    wire [63:0] M64[0:15];
    reg  [63:0] M[0:15];

    wire [63:0] M0[0:15], M1[0:15];
    wire [63:0] Q0[0:15], Q1[0:31];
    wire [63:0] XL64, XH64;
    wire [63:0] HO[0:15];

    reg [1023:0] hash_q;

    assign hash = hash_q;

    always @ (posedge clk)
        hash_q <= { HO[15], HO[14], HO[13], HO[12], HO[11], HO[10], HO[ 9], HO[ 8], HO[ 7], HO[ 6], HO[ 5], HO[ 4], HO[ 3], HO[ 2], HO[ 1], HO[ 0] };;

    always @ (posedge clk)
        M <= M64;

    genvar i;
    generate
        for(i=0;i<16;i=i+1) begin : pass2
            assign M64[i] = data[`IDX(i)];
        end
    endgenerate

    bmw512_f0 #(H) f0(
        .clk(clk),
        .M(M),
        .M_out(M0),
        .Q_out(Q0)
    );

    bmw512_f1 #(H) f1(
        .clk(clk),
        .M(M0),
        .Q(Q0),
        .M_out(M1),
        .Q_out(Q1),
        .XL64_out(XL64),
        .XH64_out(XH64)
    );

    bmw512_f2 f2(
        .clk(clk),
        .M(M1),
        .Q(Q1),
        .XL64(XL64),
        .XH64(XH64),
        .H_out(HO)
    );

endmodule

`undef IDX
`undef IV

module bmw512(
    input           clk,
    input [607:0]   data,
    input [31:0]    nonce,
    input [31:0]    target,
    output          match
);

    wire [1023:0] hash1, hash2;
    reg  [1023:0] data_d,  data_q;
    reg  [1023:0] hash1_d, hash1_q;
    reg  [63:0]   hash2_d, hash2_q;
    wire [607:0]  data_rev;
    wire [31:0]   nonce_rev;

    reg match_q;
    
    assign match = match_q;
    
    genvar i;
    generate
        for(i=0;i<76;i++) begin : reverse_data
            assign data_rev[ i*8 +: 8 ] = data[ (75-i)*8 +: 8 ];
        end
        for(i=0;i<4;i++) begin : reverse_nonce
            assign nonce_rev[ i*8 +: 8 ] = nonce[ (3-i)*8 +: 8 ];
        end
    endgenerate
    
    always @ (*)
    begin
        data_d  = { 384'h000000000000028000000000000000000000000000000000000000000000000000000000000000000000000000000080, nonce_rev[31:0], data_rev[607:0] }; 
        hash1_d = hash1;
        hash2_d = hash2[767:704];
    end
    
    always @ (posedge clk)
    begin
        data_q  <= data_d;
        hash1_q <= hash1_d;
        hash2_q <= hash2_d;
    end

    localparam [63:0] iv512[0:15] = {
        64'h8081828384858687, 64'h88898a8b8c8d8e8f,
        64'h9091929394959697, 64'h98999a9b9c9d9e9f,
        64'ha0a1a2a3a4a5a6a7, 64'ha8a9aaabacadaeaf,
        64'hb0b1b2b3b4b5b6b7, 64'hb8b9babbbcbdbebf,
        64'hc0c1c2c3c4c5c6c7, 64'hc8c9cacbcccdcecf,
        64'hd0d1d2d3d4d5d6d7, 64'hd8d9dadbdcdddedf,
        64'he0e1e2e3e4e5e6e7, 64'he8e9eaebecedeeef,
        64'hf0f1f2f3f4f5f6f7, 64'hf8f9fafbfcfdfeff
    };
    
    localparam [63:0] const512[0:15] = {
          64'haaaaaaaaaaaaaaa0,  64'haaaaaaaaaaaaaaa1,
          64'haaaaaaaaaaaaaaa2,  64'haaaaaaaaaaaaaaa3,
          64'haaaaaaaaaaaaaaa4,  64'haaaaaaaaaaaaaaa5,
          64'haaaaaaaaaaaaaaa6,  64'haaaaaaaaaaaaaaa7,
          64'haaaaaaaaaaaaaaa8,  64'haaaaaaaaaaaaaaa9,
          64'haaaaaaaaaaaaaaaa,  64'haaaaaaaaaaaaaaab,
          64'haaaaaaaaaaaaaaac,  64'haaaaaaaaaaaaaaad,
          64'haaaaaaaaaaaaaaae,  64'haaaaaaaaaaaaaaaf
    };
    
    bmw512_compress #(.H(   iv512)) c0 (  .clk(clk),  .data(data_q),   .hash(hash1));
    bmw512_compress #(.H(const512)) c1 (  .clk(clk),  .data(hash1_q),  .hash(hash2));

    always @ (posedge clk)
        $display("hash2: %x", hash2_q);
    
    always @ (posedge clk)
`ifdef SIM
        if(hash2_q == 64'hb20da74be75b8bf4)
`else
        if((hash2_q[31:0] <= target) & (hash2_q[63:32] == 0))
`endif
            match_q <= 1;
        else
            match_q <= 0;

endmodule
