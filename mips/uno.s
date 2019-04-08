  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L2:
  li $t0,1
  move $v0,$t0
  j L1
  nop
L1:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
