  .globl L7
  .data
  .align 2
  .size L7, 4
L7:
  .word 2
  .ascii "g\012\000"
  .globl L1
  .data
  .align 2
  .size L1, 4
L1:
  .word 2
  .ascii "f\012\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L17:
  move $t0,$fp
  move $a0,$t0
  sw $a0,0($sp)
  jal f_4_1
  nop
  move $v0,$zero
  j L16
  nop
L16:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl g_6_1
g_6_1:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L19:
  la $t0,L7
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  jal f_4_1
  nop
  move $v0,$zero
  j L18
  nop
L18:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl f_4_1
f_4_1:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L21:
  la $t0,L1
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  jal g_6_1
  nop
  move $v0,$zero
  j L20
  nop
L20:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
