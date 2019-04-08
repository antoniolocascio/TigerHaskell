  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L18:
  li $t0,2
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal fact_6_3
  nop
  move $t0,$v0
  move $v0,$t0
  j L17
  nop
L17:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl fact_6_3
fact_6_3:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L21:
  beqz $a1,L9
  nop
L10:
  move $s0,$a1
  addi $t0,$a1,-1
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal fact1_2_3
  nop
  move $t0,$v0
  mul $t0,$s0,$t0
L11:
  move $v0,$t0
  j L20
  nop
L9:
  li $t0,1
  j L11
  nop
L20:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl fact1_2_3
fact1_2_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L23:
  beqz $a1,L1
  nop
L2:
  move $t0,$a1
L3:
  move $v0,$t0
  j L22
  nop
L1:
  li $t0,1
  j L3
  nop
L22:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
