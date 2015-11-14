Only 3 observed types of mandatory stall conditions (given forwarding):

1. Load followed by ALU (or any operation) that uses load result (1 stall)
2. Load followed by conditional branch or indirect jump that uses load result (2 stalls)
3. ALU op followed by conditional branch that uses ALU result (1 stall)
