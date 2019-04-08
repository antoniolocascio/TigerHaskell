  .globl L1
  .data
  .align 2
  .size L1, 4
L1:
  .word 12
  .ascii "Hola Mundo!\012\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L5:
  la $t0,L1
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $v0,$zero
  j L4
  nop
L4:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
