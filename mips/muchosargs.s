  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-48
L16:
  li $t7,1
  li $t6,2
  li $t5,3
  li $t4,4
  li $t0,5
  move $t3,$t0
  li $t0,6
  move $t2,$t0
  li $t0,7
  move $t1,$t0
  li $t0,8
  move $t8,$fp
  move $a0,$t8
  sw $a0,0($sp)
  move $a1,$t7
  sw $a1,4($sp)
  move $a2,$t6
  sw $a2,8($sp)
  move $a3,$t5
  sw $a3,12($sp)
  sw $t4,16($sp)
  sw $t3,20($sp)
  sw $t2,24($sp)
  sw $t1,28($sp)
  sw $t0,32($sp)
  jal f_2_5
  nop
  move $t0,$v0
  move $v0,$t0
  j L15
  nop
L15:

  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl f_2_5
f_2_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L18:
  lw $t0,32($fp)
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
  nop
  move $v0,$zero
  j L17
  nop
L17:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
