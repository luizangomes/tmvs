* Standard prelude for initialising registers:
  0:     LD  6,       0(0) load gp with maxaddress
  1:    LDA  5,       0(6) copy gp to fp
  2:     ST  0,       0(0) clear location 0
  3:    LDA  5,       0(5) set new frame
  4:    LDA  0,       1(7) load ac with return address
  5:    LDA  7,      45(7) jump to function `main'
* Call main (deferred) and end
  6:   HALT  0,0,0 
* Code for input function
  7:     ST  0,      -1(5) store return
  8:     IN  0,0,0 input
  9:     LD  7,      -1(5) return to caller
* Code for output function
 10:     ST  0,      -1(5) store return
 11:     LD  0,      -2(5) load output value
 12:    OUT  0,0,0 output
 13:     LD  7,      -1(5) return to caller
* -> function definition: gcd
 14:     ST  0,      -1(5) store return
* -> if
* -> op
 15:     LD  0,      -3(5) load id value
 16:     ST  0,      -4(5) op: push left operand
 17:    LDC  0,       0(0) load numeric constant
 18:     LD  1,      -4(5) op: load left operand
 19:    SUB  0,1,0 op ==
 20:    JEQ  0,       2(7) jump if true
 21:    LDC  0,       0(0) false case
 22:    LDA  7,       1(7) jump to end
 23:    LDC  0,       1(0) true case
* <- op
* if: jump to else (or end) deferred
* if: then
* -> return
 24:    JEQ  0,       3(7) jump to else
 25:     LD  0,      -2(5) load id value
 26:     LD  7,      -1(5) return to caller
* <- return
* if: jump to end deferred
 27:    LDA  7,      22(7) jump to end
* if: else
* -> return
* -> call of function: gcd
 28:     LD  0,      -3(5) load id value
 29:     ST  0,      -6(5) store arg val
* -> op
 30:     LD  0,      -2(5) load id value
 31:     ST  0,      -7(5) op: push left operand
* -> op
* -> op
 32:     LD  0,      -2(5) load id value
 33:     ST  0,      -8(5) op: push left operand
 34:     LD  0,      -3(5) load id value
 35:     LD  1,      -8(5) op: load left operand
 36:    DIV  0,1,0 op /
* <- op
 37:     ST  0,      -8(5) op: push left operand
 38:     LD  0,      -3(5) load id value
 39:     LD  1,      -8(5) op: load left operand
 40:    MUL  0,1,0 op *
* <- op
 41:     LD  1,      -7(5) op: load left operand
 42:    SUB  0,1,0 op -
* <- op
 43:     ST  0,      -7(5) store arg val
 44:     ST  5,      -4(5) push old frame pointer
 45:    LDA  5,      -4(5) set new frame
 46:    LDA  0,       1(7) load ac with return address
 47:    LDA  7,     -34(7) jump to function
 48:     LD  5,       0(5) pop frame
* <- call of function: gcd
 49:     LD  7,      -1(5) return to caller
* <- return
* <- if
 50:     LD  7,      -1(5) return to caller
* <- function definition: gcd
* -> function definition: main
 51:     ST  0,      -1(5) store return
* -> assign
 52:    LDA  0,      -2(5) load id address
 53:     ST  0,      -4(5) assign: store address
 54:    LDC  0,      15(0) load numeric constant
 55:     LD  1,      -4(5) assign: load address
 56:     ST  0,       0(1) assign: store value
* <- assign
* -> assign
 57:    LDA  0,      -3(5) load id address
 58:     ST  0,      -4(5) assign: store address
 59:    LDC  0,      10(0) load numeric constant
 60:     LD  1,      -4(5) assign: load address
 61:     ST  0,       0(1) assign: store value
* <- assign
* -> call of function: output
* -> call of function: gcd
 62:     LD  0,      -2(5) load id value
 63:     ST  0,      -8(5) store arg val
 64:     LD  0,      -3(5) load id value
 65:     ST  0,      -9(5) store arg val
 66:     ST  5,      -6(5) push old frame pointer
 67:    LDA  5,      -6(5) set new frame
 68:    LDA  0,       1(7) load ac with return address
 69:    LDA  7,     -56(7) jump to function
 70:     LD  5,       0(5) pop frame
* <- call of function: gcd
 71:     ST  0,      -6(5) store arg val
 72:     ST  5,      -4(5) push old frame pointer
 73:    LDA  5,      -4(5) set new frame
 74:    LDA  0,       1(7) load ac with return address
 75:    LDA  7,     -66(7) jump to function
 76:     LD  5,       0(5) pop frame
* <- call of function: output
 77:     LD  7,      -1(5) return to caller
* <- function definition: main
* Backpatch call of main
