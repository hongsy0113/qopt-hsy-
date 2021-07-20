#ifndef CIRCUIT_CUH
#define CIRCUIT_CUH

// Generated from Cirq v0.8.0
// Qubits: [0, 1, 2, 3, 4, 5]
__device__ __inline__ void circuit(double* dm_real, double* dm_imag)
{
	H(0);
	H(1);
	H(2);
	H(3);
	H(4);
	H(5);
// Gate: ZZ**-0.9153964902652879
	RZ(-2.87580288905, 0);
	RZ(-2.87580288905, 1);
	U3(1.57079632679, 0, 0, 0);
	U3(1.57079632679, 3.14159265359, 0, 1);
	RX(1.57079632679, 0);
	CX(0, 1);
	RX(1.30500656225, 0);
	RY(1.57079632679, 1);
	CX(1, 0);
	RX(-1.57079632679, 1);
	RZ(1.57079632679, 1);
	CX(0, 1);
	U3(1.57079632679, 2.87580288905, 3.14159265359, 0);
	U3(1.57079632679, 2.87580288905, 0, 1);
// Gate: ZZ**-0.9153964902652879
	RZ(-2.87580288905, 0);
	RZ(-2.87580288905, 2);
	U3(1.57079632679, 0, 0, 0);
	U3(1.57079632679, 3.14159265359, 0, 2);
	RX(1.57079632679, 0);
	CX(0, 2);
	RX(1.30500656225, 0);
	RY(1.57079632679, 2);
	CX(2, 0);
	RX(-1.57079632679, 2);
	RZ(1.57079632679, 2);
	CX(0, 2);
	U3(1.57079632679, 2.87580288905, 3.14159265359, 0);
	U3(1.57079632679, 2.87580288905, 0, 2);
// Gate: ZZ**-0.9153964902652879
	RZ(-2.87580288905, 0);
	RZ(-2.87580288905, 5);
	U3(1.57079632679, 0, 0, 0);
	U3(1.57079632679, 3.14159265359, 0, 5);
	RX(1.57079632679, 0);
	CX(0, 5);
	RX(1.30500656225, 0);
	RY(1.57079632679, 5);
	CX(5, 0);
	RX(-1.57079632679, 5);
	RZ(1.57079632679, 5);
	CX(0, 5);
	U3(1.57079632679, 2.87580288905, 3.14159265359, 0);
	U3(1.57079632679, 2.87580288905, 0, 5);
// Gate: ZZ**-0.9153964902652879
	RZ(-2.87580288905, 1);
	RZ(-2.87580288905, 2);
	U3(1.57079632679, 0, 0, 1);
	U3(1.57079632679, 3.14159265359, 0, 2);
	RX(1.57079632679, 1);
	CX(1, 2);
	RX(1.30500656225, 1);
	RY(1.57079632679, 2);
	CX(2, 1);
	RX(-1.57079632679, 2);
	RZ(1.57079632679, 2);
	CX(1, 2);
	U3(1.57079632679, 2.87580288905, 3.14159265359, 1);
	U3(1.57079632679, 2.87580288905, 0, 2);
// Gate: ZZ**-0.9153964902652879
	RZ(-2.87580288905, 1);
	RZ(-2.87580288905, 3);
	U3(1.57079632679, 0, 0, 1);
	U3(1.57079632679, 3.14159265359, 0, 3);
	RX(1.57079632679, 1);
	CX(1, 3);
	RX(1.30500656225, 1);
	RY(1.57079632679, 3);
	CX(3, 1);
	RX(-1.57079632679, 3);
	RZ(1.57079632679, 3);
	CX(1, 3);
	U3(1.57079632679, 2.87580288905, 3.14159265359, 1);
	U3(1.57079632679, 2.87580288905, 0, 3);
// Gate: ZZ**-0.9153964902652879
	RZ(-2.87580288905, 2);
	RZ(-2.87580288905, 4);
	U3(1.57079632679, 0, 0, 2);
	U3(1.57079632679, 3.14159265359, 0, 4);
	RX(1.57079632679, 2);
	CX(2, 4);
	RX(1.30500656225, 2);
	RY(1.57079632679, 4);
	CX(4, 2);
	RX(-1.57079632679, 4);
	RZ(1.57079632679, 4);
	CX(2, 4);
	U3(1.57079632679, 2.87580288905, 3.14159265359, 2);
	U3(1.57079632679, 2.87580288905, 0, 4);
	RX(-1.98571698566, 0);
// Gate: ZZ**-0.9153964902652879
	RZ(-2.87580288905, 4);
	RZ(-2.87580288905, 3);
	U3(1.57079632679, 0, 0, 4);
	U3(1.57079632679, 3.14159265359, 0, 3);
	RX(1.57079632679, 4);
	CX(4, 3);
	RX(1.30500656225, 4);
	RY(1.57079632679, 3);
	CX(3, 4);
	RX(-1.57079632679, 3);
	RZ(1.57079632679, 3);
	CX(4, 3);
	U3(1.57079632679, 2.87580288905, 3.14159265359, 4);
	U3(1.57079632679, 2.87580288905, 0, 3);
	RX(-1.98571698566, 1);
	RX(-1.98571698566, 2);
// Gate: ZZ**-0.9153964902652879
	RZ(-2.87580288905, 4);
	RZ(-2.87580288905, 5);
	U3(1.57079632679, 0, 0, 4);
	U3(1.57079632679, 3.14159265359, 0, 5);
	RX(1.57079632679, 4);
	CX(4, 5);
	RX(1.30500656225, 4);
	RY(1.57079632679, 5);
	CX(5, 4);
	RX(-1.57079632679, 5);
	RZ(1.57079632679, 5);
	CX(4, 5);
	U3(1.57079632679, 2.87580288905, 3.14159265359, 4);
	U3(1.57079632679, 2.87580288905, 0, 5);
// Gate: ZZ**0.14873770971193984
	RZ(0.467273296105, 0);
	RZ(0.467273296105, 1);
	U3(1.57079632679, 0, 5.65915874652, 0);
	U3(1.57079632679, 3.14159265359, 5.65915874652, 1);
	RX(1.57079632679, 0);
	CX(0, 1);
	RX(1.10352303069, 0);
	RY(1.57079632679, 1);
	CX(1, 0);
	RX(-1.57079632679, 1);
	RZ(1.57079632679, 1);
	CX(0, 1);
	U3(1.57079632679, 3.29834591815, 3.14159265359, 0);
	U3(1.57079632679, 3.29834591815, 0, 1);
// Gate: ZZ**-0.9153964902652879
	RZ(-2.87580288905, 3);
	RZ(-2.87580288905, 5);
	U3(1.57079632679, 0, 0, 3);
	U3(1.57079632679, 3.14159265359, 0, 5);
	RX(1.57079632679, 3);
	CX(3, 5);
	RX(1.30500656225, 3);
	RY(1.57079632679, 5);
	CX(5, 3);
	RX(-1.57079632679, 5);
	RZ(1.57079632679, 5);
	CX(3, 5);
	U3(1.57079632679, 2.87580288905, 3.14159265359, 3);
	U3(1.57079632679, 2.87580288905, 0, 5);
	RX(-1.98571698566, 4);
// Gate: ZZ**0.14873770971193984
	RZ(0.467273296105, 0);
	RZ(0.467273296105, 2);
	U3(1.57079632679, 0, 5.65915874652, 0);
	U3(1.57079632679, 3.14159265359, 5.65915874652, 2);
	RX(1.57079632679, 0);
	CX(0, 2);
	RX(1.10352303069, 0);
	RY(1.57079632679, 2);
	CX(2, 0);
	RX(-1.57079632679, 2);
	RZ(1.57079632679, 2);
	CX(0, 2);
	U3(1.57079632679, 3.29834591815, 3.14159265359, 0);
	U3(1.57079632679, 3.29834591815, 0, 2);
	RX(-1.98571698566, 3);
	RX(-1.98571698566, 5);
// Gate: ZZ**0.14873770971193984
	RZ(0.467273296105, 1);
	RZ(0.467273296105, 2);
	U3(1.57079632679, 0, 5.65915874652, 1);
	U3(1.57079632679, 3.14159265359, 5.65915874652, 2);
	RX(1.57079632679, 1);
	CX(1, 2);
	RX(1.10352303069, 1);
	RY(1.57079632679, 2);
	CX(2, 1);
	RX(-1.57079632679, 2);
	RZ(1.57079632679, 2);
	CX(1, 2);
	U3(1.57079632679, 3.29834591815, 3.14159265359, 1);
	U3(1.57079632679, 3.29834591815, 0, 2);
// Gate: ZZ**0.14873770971193984
	RZ(0.467273296105, 0);
	RZ(0.467273296105, 5);
	U3(1.57079632679, 0, 5.65915874652, 0);
	U3(1.57079632679, 3.14159265359, 5.65915874652, 5);
	RX(1.57079632679, 0);
	CX(0, 5);
	RX(1.10352303069, 0);
	RY(1.57079632679, 5);
	CX(5, 0);
	RX(-1.57079632679, 5);
	RZ(1.57079632679, 5);
	CX(0, 5);
	U3(1.57079632679, 3.29834591815, 3.14159265359, 0);
	U3(1.57079632679, 3.29834591815, 0, 5);
// Gate: ZZ**0.14873770971193984
	RZ(0.467273296105, 1);
	RZ(0.467273296105, 3);
	U3(1.57079632679, 0, 5.65915874652, 1);
	U3(1.57079632679, 3.14159265359, 5.65915874652, 3);
	RX(1.57079632679, 1);
	CX(1, 3);
	RX(1.10352303069, 1);
	RY(1.57079632679, 3);
	CX(3, 1);
	RX(-1.57079632679, 3);
	RZ(1.57079632679, 3);
	CX(1, 3);
	U3(1.57079632679, 3.29834591815, 3.14159265359, 1);
	U3(1.57079632679, 3.29834591815, 0, 3);
// Gate: ZZ**0.14873770971193984
	RZ(0.467273296105, 2);
	RZ(0.467273296105, 4);
	U3(1.57079632679, 0, 5.65915874652, 2);
	U3(1.57079632679, 3.14159265359, 5.65915874652, 4);
	RX(1.57079632679, 2);
	CX(2, 4);
	RX(1.10352303069, 2);
	RY(1.57079632679, 4);
	CX(4, 2);
	RX(-1.57079632679, 4);
	RZ(1.57079632679, 4);
	CX(2, 4);
	U3(1.57079632679, 3.29834591815, 3.14159265359, 2);
	U3(1.57079632679, 3.29834591815, 0, 4);
// Gate: ZZ**0.14873770971193984
	RZ(0.467273296105, 4);
	RZ(0.467273296105, 3);
	U3(1.57079632679, 0, 5.65915874652, 4);
	U3(1.57079632679, 3.14159265359, 5.65915874652, 3);
	RX(1.57079632679, 4);
	CX(4, 3);
	RX(1.10352303069, 4);
	RY(1.57079632679, 3);
	CX(3, 4);
	RX(-1.57079632679, 3);
	RZ(1.57079632679, 3);
	CX(4, 3);
	U3(1.57079632679, 3.29834591815, 3.14159265359, 4);
	U3(1.57079632679, 3.29834591815, 0, 3);
	RX(-2.10803596252, 0);
	RX(-2.10803596252, 1);
	RX(-2.10803596252, 2);
// Gate: ZZ**0.14873770971193984
	RZ(0.467273296105, 4);
	RZ(0.467273296105, 5);
	U3(1.57079632679, 0, 5.65915874652, 4);
	U3(1.57079632679, 3.14159265359, 5.65915874652, 5);
	RX(1.57079632679, 4);
	CX(4, 5);
	RX(1.10352303069, 4);
	RY(1.57079632679, 5);
	CX(5, 4);
	RX(-1.57079632679, 5);
	RZ(1.57079632679, 5);
	CX(4, 5);
	U3(1.57079632679, 3.29834591815, 3.14159265359, 4);
	U3(1.57079632679, 3.29834591815, 0, 5);
// Gate: ZZ**0.14873770971193984
	RZ(0.467273296105, 3);
	RZ(0.467273296105, 5);
	U3(1.57079632679, 0, 5.65915874652, 3);
	U3(1.57079632679, 3.14159265359, 5.65915874652, 5);
	RX(1.57079632679, 3);
	CX(3, 5);
	RX(1.10352303069, 3);
	RY(1.57079632679, 5);
	CX(5, 3);
	RX(-1.57079632679, 5);
	RZ(1.57079632679, 5);
	CX(3, 5);
	U3(1.57079632679, 3.29834591815, 3.14159265359, 3);
	U3(1.57079632679, 3.29834591815, 0, 5);
	RX(-2.10803596252, 4);
	RX(-2.10803596252, 3);
	RX(-2.10803596252, 5);
}
#endif