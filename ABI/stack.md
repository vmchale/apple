On x64, the stack has to be "16-byte aligned." `malloc` (for instance) uses
this. However, the `call` instruction pushes an address (8 bytes) onto the stack, so it is 8 (mod 16) at the beginning of a procedure.

- https://stackoverflow.com/a/19128544
- community.intel.com/t5/Intel-ISA-Extensions/About-the-x64-stack-Alignment/m-p/881087#M2501

# Apple

In Apple procedures, the stack pointer should be `= 0 (mod 16)` at all times.
