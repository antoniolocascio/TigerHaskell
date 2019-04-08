  .globl L1
  .data
  .align 2
  .size L1, 4
L1:
  .word 4
  .ascii "Hola\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L10:
  move $t0,$fp
  move $a0,$t0
  sw $a0,0($sp)
  jal f_2_3
  nop
  li $t0,1
  move $v0,$t0
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
  .globl f_2_3
f_2_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L12:
  la $t0,L1
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $v0,$zero
  j L11
  nop
L11:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
