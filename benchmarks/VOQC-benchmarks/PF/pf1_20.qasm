OPENQASM 2.0;
include "qelib1.inc";
qreg q[20];
h q[0];
h q[1];
cx q[1], q[0];
rz(4.97358886312869e-09*pi) q[0];
cx q[1], q[0];
h q[1];
h q[0];
h q[1];
h q[2];
cx q[2], q[1];
rz(4.97358886312869e-09*pi) q[1];
cx q[2], q[1];
h q[2];
h q[1];
h q[2];
h q[3];
cx q[3], q[2];
rz(4.97358886312869e-09*pi) q[2];
cx q[3], q[2];
h q[3];
h q[2];
h q[3];
h q[4];
cx q[4], q[3];
rz(4.97358886312869e-09*pi) q[3];
cx q[4], q[3];
h q[4];
h q[3];
h q[4];
h q[5];
cx q[5], q[4];
rz(4.97358886312869e-09*pi) q[4];
cx q[5], q[4];
h q[5];
h q[4];
h q[5];
h q[6];
cx q[6], q[5];
rz(4.97358886312869e-09*pi) q[5];
cx q[6], q[5];
h q[6];
h q[5];
h q[6];
h q[7];
cx q[7], q[6];
rz(4.97358886312869e-09*pi) q[6];
cx q[7], q[6];
h q[7];
h q[6];
h q[7];
h q[8];
cx q[8], q[7];
rz(4.97358886312869e-09*pi) q[7];
cx q[8], q[7];
h q[8];
h q[7];
h q[8];
h q[9];
cx q[9], q[8];
rz(4.97358886312869e-09*pi) q[8];
cx q[9], q[8];
h q[9];
h q[8];
h q[9];
h q[10];
cx q[10], q[9];
rz(4.97358886312869e-09*pi) q[9];
cx q[10], q[9];
h q[10];
h q[9];
h q[10];
h q[11];
cx q[11], q[10];
rz(4.97358886312869e-09*pi) q[10];
cx q[11], q[10];
h q[11];
h q[10];
h q[11];
h q[12];
cx q[12], q[11];
rz(4.97358886312869e-09*pi) q[11];
cx q[12], q[11];
h q[12];
h q[11];
h q[12];
h q[13];
cx q[13], q[12];
rz(4.97358886312869e-09*pi) q[12];
cx q[13], q[12];
h q[13];
h q[12];
h q[13];
h q[14];
cx q[14], q[13];
rz(4.97358886312869e-09*pi) q[13];
cx q[14], q[13];
h q[14];
h q[13];
h q[14];
h q[15];
cx q[15], q[14];
rz(4.97358886312869e-09*pi) q[14];
cx q[15], q[14];
h q[15];
h q[14];
h q[15];
h q[16];
cx q[16], q[15];
rz(4.97358886312869e-09*pi) q[15];
cx q[16], q[15];
h q[16];
h q[15];
h q[16];
h q[17];
cx q[17], q[16];
rz(4.97358886312869e-09*pi) q[16];
cx q[17], q[16];
h q[17];
h q[16];
h q[17];
h q[18];
cx q[18], q[17];
rz(4.97358886312869e-09*pi) q[17];
cx q[18], q[17];
h q[18];
h q[17];
h q[18];
h q[19];
cx q[19], q[18];
rz(4.97358886312869e-09*pi) q[18];
cx q[19], q[18];
h q[19];
h q[18];
h q[0];
h q[19];
cx q[19], q[0];
rz(4.97358886312869e-09*pi) q[0];
cx q[19], q[0];
h q[19];
h q[0];
sdg q[0];
h q[0];
sdg q[0];
sdg q[1];
h q[1];
sdg q[1];
cx q[1], q[0];
rz(4.97358886312869e-09*pi) q[0];
cx q[1], q[0];
s q[1];
h q[1];
s q[1];
s q[0];
h q[0];
s q[0];
sdg q[1];
h q[1];
sdg q[1];
sdg q[2];
h q[2];
sdg q[2];
cx q[2], q[1];
rz(4.97358886312869e-09*pi) q[1];
cx q[2], q[1];
s q[2];
h q[2];
s q[2];
s q[1];
h q[1];
s q[1];
sdg q[2];
h q[2];
sdg q[2];
sdg q[3];
h q[3];
sdg q[3];
cx q[3], q[2];
rz(4.97358886312869e-09*pi) q[2];
cx q[3], q[2];
s q[3];
h q[3];
s q[3];
s q[2];
h q[2];
s q[2];
sdg q[3];
h q[3];
sdg q[3];
sdg q[4];
h q[4];
sdg q[4];
cx q[4], q[3];
rz(4.97358886312869e-09*pi) q[3];
cx q[4], q[3];
s q[4];
h q[4];
s q[4];
s q[3];
h q[3];
s q[3];
sdg q[4];
h q[4];
sdg q[4];
sdg q[5];
h q[5];
sdg q[5];
cx q[5], q[4];
rz(4.97358886312869e-09*pi) q[4];
cx q[5], q[4];
s q[5];
h q[5];
s q[5];
s q[4];
h q[4];
s q[4];
sdg q[5];
h q[5];
sdg q[5];
sdg q[6];
h q[6];
sdg q[6];
cx q[6], q[5];
rz(4.97358886312869e-09*pi) q[5];
cx q[6], q[5];
s q[6];
h q[6];
s q[6];
s q[5];
h q[5];
s q[5];
sdg q[6];
h q[6];
sdg q[6];
sdg q[7];
h q[7];
sdg q[7];
cx q[7], q[6];
rz(4.97358886312869e-09*pi) q[6];
cx q[7], q[6];
s q[7];
h q[7];
s q[7];
s q[6];
h q[6];
s q[6];
sdg q[7];
h q[7];
sdg q[7];
sdg q[8];
h q[8];
sdg q[8];
cx q[8], q[7];
rz(4.97358886312869e-09*pi) q[7];
cx q[8], q[7];
s q[8];
h q[8];
s q[8];
s q[7];
h q[7];
s q[7];
sdg q[8];
h q[8];
sdg q[8];
sdg q[9];
h q[9];
sdg q[9];
cx q[9], q[8];
rz(4.97358886312869e-09*pi) q[8];
cx q[9], q[8];
s q[9];
h q[9];
s q[9];
s q[8];
h q[8];
s q[8];
sdg q[9];
h q[9];
sdg q[9];
sdg q[10];
h q[10];
sdg q[10];
cx q[10], q[9];
rz(4.97358886312869e-09*pi) q[9];
cx q[10], q[9];
s q[10];
h q[10];
s q[10];
s q[9];
h q[9];
s q[9];
sdg q[10];
h q[10];
sdg q[10];
sdg q[11];
h q[11];
sdg q[11];
cx q[11], q[10];
rz(4.97358886312869e-09*pi) q[10];
cx q[11], q[10];
s q[11];
h q[11];
s q[11];
s q[10];
h q[10];
s q[10];
sdg q[11];
h q[11];
sdg q[11];
sdg q[12];
h q[12];
sdg q[12];
cx q[12], q[11];
rz(4.97358886312869e-09*pi) q[11];
cx q[12], q[11];
s q[12];
h q[12];
s q[12];
s q[11];
h q[11];
s q[11];
sdg q[12];
h q[12];
sdg q[12];
sdg q[13];
h q[13];
sdg q[13];
cx q[13], q[12];
rz(4.97358886312869e-09*pi) q[12];
cx q[13], q[12];
s q[13];
h q[13];
s q[13];
s q[12];
h q[12];
s q[12];
sdg q[13];
h q[13];
sdg q[13];
sdg q[14];
h q[14];
sdg q[14];
cx q[14], q[13];
rz(4.97358886312869e-09*pi) q[13];
cx q[14], q[13];
s q[14];
h q[14];
s q[14];
s q[13];
h q[13];
s q[13];
sdg q[14];
h q[14];
sdg q[14];
sdg q[15];
h q[15];
sdg q[15];
cx q[15], q[14];
rz(4.97358886312869e-09*pi) q[14];
cx q[15], q[14];
s q[15];
h q[15];
s q[15];
s q[14];
h q[14];
s q[14];
sdg q[15];
h q[15];
sdg q[15];
sdg q[16];
h q[16];
sdg q[16];
cx q[16], q[15];
rz(4.97358886312869e-09*pi) q[15];
cx q[16], q[15];
s q[16];
h q[16];
s q[16];
s q[15];
h q[15];
s q[15];
sdg q[16];
h q[16];
sdg q[16];
sdg q[17];
h q[17];
sdg q[17];
cx q[17], q[16];
rz(4.97358886312869e-09*pi) q[16];
cx q[17], q[16];
s q[17];
h q[17];
s q[17];
s q[16];
h q[16];
s q[16];
sdg q[17];
h q[17];
sdg q[17];
sdg q[18];
h q[18];
sdg q[18];
cx q[18], q[17];
rz(4.97358886312869e-09*pi) q[17];
cx q[18], q[17];
s q[18];
h q[18];
s q[18];
s q[17];
h q[17];
s q[17];
sdg q[18];
h q[18];
sdg q[18];
sdg q[19];
h q[19];
sdg q[19];
cx q[19], q[18];
rz(4.97358886312869e-09*pi) q[18];
cx q[19], q[18];
s q[19];
h q[19];
s q[19];
s q[18];
h q[18];
s q[18];
sdg q[0];
h q[0];
sdg q[0];
sdg q[19];
h q[19];
sdg q[19];
cx q[19], q[0];
rz(4.97358886312869e-09*pi) q[0];
cx q[19], q[0];
s q[19];
h q[19];
s q[19];
s q[0];
h q[0];
s q[0];
cx q[1], q[0];
rz(4.97358886312869e-09*pi) q[0];
cx q[1], q[0];
cx q[2], q[1];
rz(4.97358886312869e-09*pi) q[1];
cx q[2], q[1];
cx q[3], q[2];
rz(4.97358886312869e-09*pi) q[2];
cx q[3], q[2];
cx q[4], q[3];
rz(4.97358886312869e-09*pi) q[3];
cx q[4], q[3];
cx q[5], q[4];
rz(4.97358886312869e-09*pi) q[4];
cx q[5], q[4];
cx q[6], q[5];
rz(4.97358886312869e-09*pi) q[5];
cx q[6], q[5];
cx q[7], q[6];
rz(4.97358886312869e-09*pi) q[6];
cx q[7], q[6];
cx q[8], q[7];
rz(4.97358886312869e-09*pi) q[7];
cx q[8], q[7];
cx q[9], q[8];
rz(4.97358886312869e-09*pi) q[8];
cx q[9], q[8];
cx q[10], q[9];
rz(4.97358886312869e-09*pi) q[9];
cx q[10], q[9];
cx q[11], q[10];
rz(4.97358886312869e-09*pi) q[10];
cx q[11], q[10];
cx q[12], q[11];
rz(4.97358886312869e-09*pi) q[11];
cx q[12], q[11];
cx q[13], q[12];
rz(4.97358886312869e-09*pi) q[12];
cx q[13], q[12];
cx q[14], q[13];
rz(4.97358886312869e-09*pi) q[13];
cx q[14], q[13];
cx q[15], q[14];
rz(4.97358886312869e-09*pi) q[14];
cx q[15], q[14];
cx q[16], q[15];
rz(4.97358886312869e-09*pi) q[15];
cx q[16], q[15];
cx q[17], q[16];
rz(4.97358886312869e-09*pi) q[16];
cx q[17], q[16];
cx q[18], q[17];
rz(4.97358886312869e-09*pi) q[17];
cx q[18], q[17];
cx q[19], q[18];
rz(4.97358886312869e-09*pi) q[18];
cx q[19], q[18];
cx q[19], q[0];
rz(4.97358886312869e-09*pi) q[0];
cx q[19], q[0];
rz(3.8689734317312037e-10*pi) q[0];
rz(4.475782368467127e-09*pi) q[1];
rz(3.0765888032141935e-09*pi) q[2];
rz(1.0386960418930315e-09*pi) q[3];
rz(4.5802250728949756e-09*pi) q[4];
rz(2.7068452532939344e-10*pi) q[5];
rz(4.463010615995629e-09*pi) q[6];
rz(2.5485785314214546e-09*pi) q[7];
rz(1.2106232763172987e-09*pi) q[8];
rz(2.0880127650659655e-09*pi) q[9];
rz(3.5573443260562726e-09*pi) q[10];
rz(2.277582706899788e-09*pi) q[11];
rz(4.967564097738859e-09*pi) q[12];
rz(4.557277470202961e-09*pi) q[13];
rz(3.207844539825064e-09*pi) q[14];
rz(3.1441776369480605e-09*pi) q[15];
rz(2.153795686041176e-09*pi) q[16];
rz(4.205509504396919e-09*pi) q[17];
rz(2.7338895370852314e-09*pi) q[18];
rz(1.277828405173543e-09*pi) q[19];
