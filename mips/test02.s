  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L5:
  li $t0,10
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $v0,$zero
  j L4
  nop
L4:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
