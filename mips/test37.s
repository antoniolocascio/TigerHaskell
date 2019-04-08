  .globl L2
  .data
  .align 2
  .size L2, 4
L2:
  .word 1
  .ascii " \000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L5:
  move $v0,$zero
  j L4
  nop
L4:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
