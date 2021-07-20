OPENQASM 2.0;
include "qelib1.inc";

qreg r[1];
qreg q[2];		// create a quantum register with 2 qubits
creg c[2]; 		// create a classical register with 2 bits
h q[0];  		// perform Hadamard gate on one qubit
h q[0];  		// perform Hadamard gate on one qubit
h q[0];  		// perform Hadamard gate on one qubit
h q[0];  		// perform Hadamard gate on one qubit
h q[0];
cx q[0],q[1];  	// perform control-not gate on both qubits

measure q[0] -> c[0];	//measure one qubit, and put outcome in one bit
measure q[1] -> c[1];	//measure the other qubit, and put outcome in the other bit
