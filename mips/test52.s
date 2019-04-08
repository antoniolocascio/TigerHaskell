  .globl L7
  .data
  .align 2
  .size L7, 4
L7:
  .word 9
  .ascii "somewhere\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-40
  sw $s0,16($sp)
  sw $s1,20($sp)
  sw $s2,24($sp)
  sw $s3,28($sp)
L21:
  li $t0,10
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  move $s3,$t0
  la $t0,L7
  move $s2,$t0
  move $s1,$zero
  move $s0,$zero
  li $t0,10
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  li $t1,4
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s2
  sw $a1,4($sp)
  move $a2,$s1
  sw $a2,8($sp)
  move $a3,$s0
  sw $a3,12($sp)
  sw $t0,16($sp)
  jal _allocRecord
  nop
  move $t0,$v0
  li $t1,5
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  move $s2,$t0
  move $s1,$s3
  move $s0,$zero
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t1,$s1,$t0
  li $t0,1
  sw $t0,0($t1)
  move $s1,$s3
  li $t0,9
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t1,$s1,$t0
  li $t0,3
  sw $t0,0($t1)
  move $s1,$s2
  li $t0,1
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t0,$s1,$t0
  lw $t0,0($t0)
  move $a0,$t0
  sw $a0,0($sp)
  jal _checkNil
  nop
  move $s1,$s2
  li $t0,1
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t0,$s1,$t0
  lw $t0,0($t0)
  addi $t1,$t0,4
  li $t0,23
  sw $t0,0($t1)
  move $v0,$zero
  j L20
  nop
L20:

  lw $s0,16($sp)
  lw $s1,20($sp)
  lw $s2,24($sp)
  lw $s3,28($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
