  .globl L6
  .data
  .align 2
  .size L6, 4
L6:
  .word 8
  .ascii "Somebody\000"
  .globl L2
  .data
  .align 2
  .size L2, 4
L2:
  .word 6
  .ascii "Nobody\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L10:
  li $t1,1000
  la $t0,L2
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
  addi $t1,$s0,4
  la $t0,L6
  sw $t0,0($t1)
  move $a0,$s0
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $t0,4($s0)
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $v0,$zero
  j L9
  nop
L9:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop
