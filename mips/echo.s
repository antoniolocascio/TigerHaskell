  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L13:
  move $t0,$fp
  move $a0,$t0
  sw $a0,0($sp)
  jal echo_2_3
  nop
  move $v0,$zero
  j L12
  nop
L12:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl echo_2_3
echo_2_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L15:
  jal getstr
  nop
  move $t0,$v0
L7:
L8:
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  jal getstr
  nop
  move $t0,$v0
  j L7
  nop
L3:
  move $v0,$zero
  j L14
  nop
L14:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
