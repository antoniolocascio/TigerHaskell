  .globl L20
  .data
  .align 2
  .size L20, 4
L20:
  .word 6
  .ascii "iter: \000"
  .globl L15
  .data
  .align 2
  .size L15, 4
L15:
  .word 14
  .ascii "for, current: \000"
  .globl L9
  .data
  .align 2
  .size L9, 4
L9:
  .word 4
  .ascii "End\012\000"
  .globl L3
  .data
  .align 2
  .size L3, 4
L3:
  .word 7
  .ascii "Entry: \000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-16($sp)
  sw $ra,-12($sp)
  move $fp,$sp
  addi $sp,$sp,-32
L41:
  li $t0,2
  addi $t1,$fp,-4
  sw $t0,0($t1)
  move $t0,$zero
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal f_4_5
  nop
  move $v0,$zero
  j L40
  nop
L40:

  move $sp,$fp
  lw $fp,-16($sp)
  lw $ra,-12($sp)
  j $ra
  nop

  .text
  .align 2
  .globl f_4_5
f_4_5:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L45:
  la $t0,L3
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  lw $a1,4($fp)
  move $t0,$a1
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
  nop
  lw $a1,4($fp)
  move $t1,$a1
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  beq $t1,$t0,L32
  nop
L33:
  move $s0,$zero
L31:
  move $t1,$s0
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  addi $t0,$t0,-1
  bgt $t1,$t0,L30
  nop
L29:
  la $t0,L15
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  lw $a1,4($fp)
  move $t0,$a1
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
  nop
  la $t0,L20
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$s0
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
  nop
  lw $a1,4($fp)
  addi $t1,$a1,1
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  jal f_4_5
  nop
  addi $t0,$s0,1
  move $s0,$t0
  j L31
  nop
L32:
  la $t0,L9
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
L34:
  move $v0,$t0
  j L44
  nop
L30:
  move $t0,$zero
  j L34
  nop
L44:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop
