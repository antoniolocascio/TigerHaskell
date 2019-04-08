  .globl L12
  .data
  .align 2
  .size L12, 4
L12:
  .word 4
  .ascii "str2\000"
  .globl L11
  .data
  .align 2
  .size L11, 4
L11:
  .word 1
  .ascii " \000"
  .globl L5
  .data
  .align 2
  .size L5, 4
L5:
  .word 3
  .ascii "str\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L19:
  move $t1,$zero
  la $t0,L12
  move $t2,$fp
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal do_nothing1_4_1
  nop
  move $t0,$v0
  move $v0,$t0
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
  .globl do_nothing2_7_1
do_nothing2_7_1:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L21:
  move $t1,$a1
  la $t0,L5
  lw $t2,0($fp)
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal do_nothing1_4_1
  nop
  la $t0,L11
  move $v0,$t0
  j L20
  nop
L20:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl do_nothing1_4_1
do_nothing1_4_1:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L23:
  addi $t0,$a1,1
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal do_nothing2_7_1
  nop
  move $v0,$zero
  j L22
  nop
L22:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
