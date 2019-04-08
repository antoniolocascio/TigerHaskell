  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L2:
  move $v0,$zero
  j L1
  nop
L1:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl g_8_9
g_8_9:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L4:
  move $v0,$a1
  j L3
  nop
L3:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl g_6_9
g_6_9:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L6:
  move $v0,$a1
  j L5
  nop
L5:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
