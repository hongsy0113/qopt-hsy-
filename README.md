# Qopt

Quantum circuit optimization with equality saturation

### Overview
#### Problem: Quantum circuit optimization 
- A Verified Optimizer for Quantum Circuits. POPL 2021
- Automated optimization of large quantum circuits with continuous parameters. Quantum Information 2018

#### Approach: Equality saturation
- egg: Fast and Extensible Equality Saturation. POPL 2021
- Equality saturation: a new approach to optimization. POPL 2009
- Equality Saturation for Tensor Graph Superoptimization. MLSys 2021

### Running

binary:
```
./build
./main.native -input benchmarks/toys/teleport.qasm

```

script:
```
python3 run_opt.py benchmarks/toys/teleport.qasm
```
