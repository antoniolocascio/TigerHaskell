  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L16:
  li $t0,10
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal fact_4_3
  nop
  move $t0,$v0
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
  nop
  move $v0,$zero
  j L15
  nop
L15:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl fact_4_3
fact_4_3:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L19:
  beqz $a1,L5
  nop
L6:
  move $s0,$a1
  addi $t0,$a1,-1
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal fact_4_3
  nop
  move $t0,$v0
  mul $t0,$s0,$t0
L7:
  move $v0,$t0
  j L18
  nop
L5:
  li $t0,1
  j L7
  nop
L18:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop
