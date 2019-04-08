  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L9:
L3:
  move $t0,$zero
L2:
  bnez $t0,L4
  nop
L5:
  li $t0,40
L6:
  move $v0,$t0
  j L8
  nop
L4:
  li $t0,30
  j L6
  nop
L8:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
