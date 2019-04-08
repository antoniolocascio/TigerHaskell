  .globl L36
  .data
  .align 2
  .size L36, 4
L36:
  .word 5
  .ascii "Mal!\012\000"
  .globl L33
  .data
  .align 2
  .size L33, 4
L33:
  .word 6
  .ascii "Bien!\012\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L44:
  li $t0,1
  move $s0,$t0
  li $t0,2
  move $t1,$zero
  move $t2,$fp
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  move $a2,$t1
  sw $a2,8($sp)
  jal cons_3_5
  nop
  move $t0,$v0
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal cons_3_5
  nop
  move $t0,$v0
  li $t1,1
  move $t2,$fp
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal isin_5_5
  nop
  move $t0,$v0
  bnez $t0,L39
  nop
L40:
  la $t0,L36
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
L41:
  move $v0,$t0
  j L43
  nop
L39:
  la $t0,L33
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
  j L41
  nop
L43:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl isin_5_5
isin_5_5:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L46:
  beqz $a2,L13
  nop
L14:
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $a2,8($fp)
  lw $t0,0($a2)
  beq $a1,$t0,L9
  nop
L10:
  move $s0,$a1
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a2,8($fp)
  lw $t0,4($a2)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal isin_5_5
  nop
  move $t0,$v0
L11:
L15:
  move $v0,$t0
  j L45
  nop
L13:
  move $t0,$zero
  j L15
  nop
L9:
  li $t0,1
  j L11
  nop
L45:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl cons_3_5
cons_3_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L48:
  move $t1,$a1
  move $t0,$a2
  li $t2,2
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  j L47
  nop
L47:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
