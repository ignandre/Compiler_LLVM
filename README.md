Description: 
This is an IR generator for the compiler that will generate LLVM IR instructions.
The pass of the compiler will traverse the abstract syntax tree, stringing together the
appropriate LLVM IR instructions for each subtree - to assign a variable, do vector
operations, or whatever is needed. Those LLVM instructions are then executed by a
modified LLVM JIT compiler without any grungy details of the machine code. 