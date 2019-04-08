  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L15:
  li $s0,1
  li $t0,2
  move $t1,$zero
  move $t2,$fp
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  move $a2,$t1
  sw $a2,8($sp)
  jal cons_3_5
  nop
  move $t0,$v0
  li $t1,2
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
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
  j L14
  nop
L14:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl cons_3_5
cons_3_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L17:
  move $t1,$a1
  move $t0,$a2
  li $t2,2
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  j L16
  nop
L16:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
