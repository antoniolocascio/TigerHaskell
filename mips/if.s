  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L10:
  li $t0,10
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal fact_2_3
  nop
  move $t0,$v0
  move $v0,$t0
  j L9
  nop
L9:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl fact_2_3
fact_2_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L12:
  beqz $a1,L1
  nop
L2:
  move $t0,$a1
L3:
  move $v0,$t0
  j L11
  nop
L1:
  li $t0,1
  j L3
  nop
L11:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
