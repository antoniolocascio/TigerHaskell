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
  move $t0,$zero
L6:
  li $t2,100
  bgt $t0,$t2,L5
  nop
L4:
  addi $t1,$t1,1
  addi $t0,$t0,1
  j L6
  nop
L5:
  move $v0,$zero
  j L7
  nop
L7:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
