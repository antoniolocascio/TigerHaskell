  .globl L3
  .data
  .align 2
  .size L3, 4
L3:
  .word 1
  .ascii "g\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L13:
  jal getstr
  nop
  move $t1,$v0
  la $t0,L3
  li $s0,1
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal _stringCompare
  nop
  move $t0,$v0
  beqz $t0,L7
  nop
L8:
  move $s0,$zero
L7:
  move $t0,$s0
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
  nop
  move $v0,$zero
  j L12
  nop
L12:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop
