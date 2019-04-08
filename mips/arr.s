  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L33:
  li $t0,100
  move $s0,$t0
  move $a0,$s0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  addi $t1,$fp,-4
  sw $t0,0($t1)
  move $t0,$s0
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal fillArr_12_5
  nop
  move $t0,$s0
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal printArr_8_5
  nop
  move $v0,$zero
  j L32
  nop
L32:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl fillArr_12_5
fillArr_12_5:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-40
  sw $s0,16($sp)
  sw $s1,20($sp)
  sw $s2,24($sp)
L35:
  move $s2,$zero
L23:
  addi $t0,$a1,-1
  bgt $s2,$t0,L22
  nop
L21:
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  move $s1,$t0
  move $s0,$s2
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
  addi $t0,$s2,1
  move $s2,$t0
  j L23
  nop
L22:
  move $v0,$zero
  j L34
  nop
L34:

  lw $s0,16($sp)
  lw $s1,20($sp)
  lw $s2,24($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl printArr_8_5
printArr_8_5:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-40
  sw $s0,16($sp)
  sw $s1,20($sp)
  sw $s2,24($sp)
L37:
  move $s2,$zero
L15:
  addi $t0,$a1,-1
  bgt $s2,$t0,L14
  nop
L13:
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  move $s1,$t0
  move $s0,$s2
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
  jal print_int
  nop
  lw $a1,4($fp)
  addi $t0,$s2,1
  move $s2,$t0
  j L15
  nop
L14:
  move $v0,$zero
  j L36
  nop
L36:

  lw $s0,16($sp)
  lw $s1,20($sp)
  lw $s2,24($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop
