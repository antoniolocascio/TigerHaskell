  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L8:
  move $t1,$zero
  beqz $t1,L3
  nop
L3:
  li $t0,1
  bnez $t1,L5
  nop
L6:
  move $t0,$zero
L5:
  move $v0,$t0
  j L7
  nop
L7:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
