  0:     LD  6,       0(0) load gp with maxaddress
  1:    LDA  5,       0(6) copy gp to fp
  2:     ST  0,       0(0) clear location 0
  3:    LDA  5,       0(5) set new frame
  4:    LDA  0,       1(7) load ac with return address
  5:    LDA  7,      45(7) jump to function `main'
  6:   HALT  0,0,0 
  7:     ST  0,      -1(5) store return --in--retaddr--
  8:     IN  0,0,0 input
  9:     LD  7,      -1(5) return to caller
 10:     ST  0,      -1(5) store return --out--retaddr--
 11:     LD  0,      -2(5) load output value
 12:    OUT  0,0,0 output
 13:     LD  7,      -1(5) return to caller
 14:     ST  0,      -1(5) store return --gcd--retaddr--
 15:     LD  0,      -3(5) load id value
 16:     ST  0,      -4(5) op: push left operand --gcd--opleft--
 17:    LDC  0,       0(0) load numeric constant
 18:     LD  1,      -4(5) op: load left operand
 19:    SUB  0,1,0 op ==
 20:    JEQ  0,       2(7) jump if true
 21:    LDC  0,       0(0) false case
 22:    LDA  7,       1(7) jump to end
 23:    LDC  0,       1(0) true case
 24:    JEQ  0,       3(7) jump to else
 25:     LD  0,      -2(5) load id value
 26:     LD  7,      -1(5) return to caller
 27:    LDA  7,      22(7) jump to end
 28:     LD  0,      -3(5) load id value
 29:     ST  0,      -6(5) store arg val --gcd--newarg--
 30:     LD  0,      -2(5) load id value
 31:     ST  0,      -7(5) op: push left operand --gcd--opleft--
 32:     LD  0,      -2(5) load id value
 33:     ST  0,      -8(5) op: push left operand --gcd--opleft--
 34:     LD  0,      -3(5) load id value
 35:     LD  1,      -8(5) op: load left operand
 36:    DIV  0,1,0 op /
 37:     ST  0,      -8(5) op: push left operand --gcd--opleft--
 38:     LD  0,      -3(5) load id value
 39:     LD  1,      -8(5) op: load left operand
 40:    MUL  0,1,0 op *
 41:     LD  1,      -7(5) op: load left operand
 42:    SUB  0,1,0 op -
 43:     ST  0,      -7(5) store arg val --gcd --newarg--
 44:     ST  5,      -4(5) push old frame pointer --gcd--set_ofp--
 45:    LDA  5,      -4(5) set new frame
 46:    LDA  0,       1(7) load ac with return address
 47:    LDA  7,     -34(7) jump to function
 48:     LD  5,       0(5) pop frame
 49:     LD  7,      -1(5) return to caller
 50:     LD  7,      -1(5) return to caller
 51:     ST  0,      -1(5) store return --main--retaddr--
 52:    LDA  0,      -2(5) load id address
 53:     ST  0,      -4(5) assign: store address --main--st_addr--
 54:     ST  5,      -5(5) push old frame pointer --main--set_ofp--
 55:    LDA  5,      -5(5) set new frame
 56:    LDA  0,       1(7) load ac with return address
 57:    LDA  7,     -51(7) jump to function
 58:     LD  5,       0(5) pop frame
 59:     LD  1,      -4(5) assign: load address
 60:     ST  0,       0(1) assign: store value --main--st_val--
 61:    LDA  0,      -3(5) load id address
 62:     ST  0,      -4(5) assign: store address --main--st_addr--
 63:     ST  5,      -5(5) push old frame pointer --main--set_ofp--
 64:    LDA  5,      -5(5) set new frame
 65:    LDA  0,       1(7) load ac with return address
 66:    LDA  7,     -60(7) jump to function
 67:     LD  5,       0(5) pop frame
 68:     LD  1,      -4(5) assign: load address
 69:     ST  0,       0(1) assign: store value --main--st_val--
 70:     LD  0,      -2(5) load id value
 71:     ST  0,      -8(5) store arg val --main--new_arg--
 72:     LD  0,      -3(5) load id value
 73:     ST  0,      -9(5) store arg val --main--new_arg--
 74:     ST  5,      -6(5) push old frame pointer --main--set_ofp--
 75:    LDA  5,      -6(5) set new frame
 76:    LDA  0,       1(7) load ac with return address
 77:    LDA  7,     -64(7) jump to function
 78:     LD  5,       0(5) pop frame
 79:     ST  0,      -6(5) store arg val --main--st_arg--
 80:     ST  5,      -4(5) push old frame pointer --main--set_ofp--
 81:    LDA  5,      -4(5) set new frame
 82:    LDA  0,       1(7) load ac with return address
 83:    LDA  7,     -74(7) jump to function
 84:     LD  5,       0(5) pop frame
 85:     LD  7,      -1(5) return to caller
