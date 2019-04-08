  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L4:
L5:
L2:
L1:
  move $v0,$zero
  j L6
  nop
L3:
  j L4
  nop
L6:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
