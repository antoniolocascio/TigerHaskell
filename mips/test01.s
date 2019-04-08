  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
  sw $s1,20($sp)
L7:
  li $t0,10
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  move $s1,$t0
  li $t0,5
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
  move $v0,$t0
  j L6
  nop
L6:

  lw $s0,16($sp)
  lw $s1,20($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
