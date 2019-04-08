  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-16($sp)
  sw $ra,-12($sp)
  move $fp,$sp
  addi $sp,$sp,-32
L24:
  move $t0,$zero
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal suc_2_5
  nop
  move $t0,$v0
  li $t1,2
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  addi $t1,$fp,-4
  sw $t0,0($t1)
  li $t0,2
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal printArr_8_5
  nop
  move $v0,$zero
  j L23
  nop
L23:

  move $sp,$fp
  lw $fp,-16($sp)
  lw $ra,-12($sp)
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
L26:
  move $s2,$zero
L18:
  addi $t0,$a1,-1
  bgt $s2,$t0,L17
  nop
L16:
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
  j L18
  nop
L17:
  move $v0,$zero
  j L25
  nop
L25:

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
  .globl suc_2_5
suc_2_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L28:
  addi $t0,$a1,1
  move $v0,$t0
  j L27
  nop
L27:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
