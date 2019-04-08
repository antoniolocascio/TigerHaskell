  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L10:
  move $t0,$zero
  li $t1,1
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal _allocRecord
  nop
  move $t0,$v0
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal a_3_5
  nop
  move $v0,$zero
  j L9
  nop
L9:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl a_3_5
a_3_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L12:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  li $t1,1
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal _allocRecord
  nop
  j L11
  nop
L11:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
