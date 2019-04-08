  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L11:
  li $t1,9
  move $t0,$zero
  li $t2,2
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  move $t0,$v0
  li $t1,1
  li $t2,2
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  move $s0,$v0
  move $a0,$s0
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $t0,4($s0)
  move $a0,$t0
  sw $a0,0($sp)
  jal _checkNil
  nop
  move $a0,$s0
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $t0,4($s0)
  lw $t0,0($t0)
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
  nop
  move $v0,$zero
  j L10
  nop
L10:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop
