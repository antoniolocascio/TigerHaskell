  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L9:
  li $t0,2
  li $t1,3
  move $t2,$fp
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  move $a2,$t1
  sw $a2,8($sp)
  jal sum_2_3
  nop
  move $t0,$v0
  move $v0,$t0
  j L8
  nop
L8:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl sum_2_3
sum_2_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L11:
  add $t0,$a1,$a2
  move $v0,$t0
  j L10
  nop
L10:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
