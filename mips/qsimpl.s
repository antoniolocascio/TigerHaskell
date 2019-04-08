  .globl L9
  .data
  .align 2
  .size L9, 4
L9:
  .word 4
  .ascii "End\012\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-16($sp)
  sw $ra,-12($sp)
  move $fp,$sp
  addi $sp,$sp,-32
L34:
  li $t0,2
  addi $t1,$fp,-4
  sw $t0,0($t1)
  lw $t0,-4($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  addi $t1,$fp,-8
  sw $t0,0($t1)
  move $t0,$zero
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal try_8_5
  nop
  move $v0,$zero
  j L33
  nop
L33:

  move $sp,$fp
  lw $fp,-16($sp)
  lw $ra,-12($sp)
  j $ra
  nop

  .text
  .align 2
  .globl try_8_5
try_8_5:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-40
  sw $s0,16($sp)
  sw $s1,20($sp)
  sw $s2,24($sp)
L38:
  move $t1,$a1
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  beq $t1,$t0,L25
  nop
L26:
  move $s2,$zero
L24:
  move $t1,$s2
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  addi $t0,$t0,-1
  bgt $t1,$t0,L23
  nop
L22:
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-8($t0)
  move $s1,$t0
  move $s0,$a1
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s0,2
  add $t0,$s1,$t0
  sw $s2,0($t0)
  addi $t1,$a1,1
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  jal try_8_5
  nop
  lw $a1,4($fp)
  addi $t0,$s2,1
  move $s2,$t0
  j L24
  nop
L25:
  la $t0,L9
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
L27:
  move $v0,$t0
  j L37
  nop
L23:
  move $t0,$zero
  j L27
  nop
L37:

  lw $s0,16($sp)
  lw $s1,20($sp)
  lw $s2,24($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop
