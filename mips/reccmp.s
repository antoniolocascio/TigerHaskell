  .globl L6
  .data
  .align 2
  .size L6, 4
L6:
  .word 1
  .ascii "a\000"
  .globl L2
  .data
  .align 2
  .size L2, 4
L2:
  .word 1
  .ascii "a\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L16:
  li $t1,1
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
  li $t1,1
  la $t0,L6
  li $t2,2
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  li $t0,1
  beq $s0,$s0,L12
  nop
L13:
  move $t0,$zero
L12:
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
  nop
  move $v0,$zero
  j L15
  nop
L15:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop
