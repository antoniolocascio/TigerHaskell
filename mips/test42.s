  .globl L44
  .data
  .align 2
  .size L44, 4
L44:
  .word 3
  .ascii "sdf\000"
  .globl L43
  .data
  .align 2
  .size L43, 4
L43:
  .word 3
  .ascii "sfd\000"
  .globl L38
  .data
  .align 2
  .size L38, 4
L38:
  .word 4
  .ascii "kati\000"
  .globl L27
  .data
  .align 2
  .size L27, 4
L27:
  .word 5
  .ascii "Allos\000"
  .globl L21
  .data
  .align 2
  .size L21, 4
L21:
  .word 5
  .ascii "Kapou\000"
  .globl L20
  .data
  .align 2
  .size L20, 4
L20:
  .word 7
  .ascii "Kapoios\000"
  .globl L17
  .data
  .align 2
  .size L17, 4
L17:
  .word 0
  .ascii "\000"
  .globl L10
  .data
  .align 2
  .size L10, 4
L10:
  .word 9
  .ascii "somewhere\000"
  .globl L9
  .data
  .align 2
  .size L9, 4
L9:
  .word 5
  .ascii "aname\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-56
  sw $s0,16($sp)
  sw $s1,20($sp)
  sw $s2,24($sp)
  sw $s3,28($sp)
  sw $s4,32($sp)
  sw $s5,36($sp)
  sw $s6,40($sp)
L50:
  li $t0,10
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  move $s6,$t0
  la $t2,L10
  move $t3,$zero
  move $t1,$zero
  la $t0,L9
  li $t4,4
  move $a0,$t4
  sw $a0,0($sp)
  move $a1,$t2
  sw $a1,4($sp)
  move $a2,$t3
  sw $a2,8($sp)
  move $a3,$t1
  sw $a3,12($sp)
  sw $t0,16($sp)
  jal _allocRecord
  nop
  move $t0,$v0
  li $t1,5
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  move $s5,$t0
  li $t0,100
  move $a0,$t0
  sw $a0,0($sp)
  la $t0,L17
  move $a1,$t0
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  move $s4,$t0
  la $t3,L21
  li $t2,44
  li $t1,2432
  la $t0,L20
  li $t4,4
  move $a0,$t4
  sw $a0,0($sp)
  move $a1,$t3
  sw $a1,4($sp)
  move $a2,$t2
  sw $a2,8($sp)
  move $a3,$t1
  sw $a3,12($sp)
  sw $t0,16($sp)
  jal _allocRecord
  nop
  move $s3,$v0
  li $t0,3
  move $a0,$t0
  sw $a0,0($sp)
  li $t0,1900
  move $a1,$t0
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t1,$v0
  la $t0,L27
  li $t2,2
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  move $s2,$v0
  move $s1,$s6
  move $s0,$zero
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t1,$s1,$t0
  li $t0,1
  sw $t0,0($t1)
  move $s1,$s6
  li $t0,9
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t1,$s1,$t0
  li $t0,3
  sw $t0,0($t1)
  move $s1,$s5
  li $t0,3
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
  move $a0,$t0
  sw $a0,0($sp)
  jal _checkNil
  nop
  move $s1,$s5
  li $t0,3
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
  addi $t1,$t0,12
  la $t0,L38
  sw $t0,0($t1)
  move $s1,$s5
  li $t0,1
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
  move $a0,$t0
  sw $a0,0($sp)
  jal _checkNil
  nop
  move $s1,$s5
  li $t0,1
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
  addi $t1,$t0,4
  li $t0,23
  sw $t0,0($t1)
  move $s1,$s4
  li $t0,34
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t1,$s1,$t0
  la $t0,L43
  sw $t0,0($t1)
  move $a0,$s3
  sw $a0,0($sp)
  jal _checkNil
  nop
  addi $t1,$s3,12
  la $t0,L44
  sw $t0,0($t1)
  move $a0,$s2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $t0,0($s2)
  move $s1,$t0
  move $s0,$zero
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t1,$s1,$t0
  li $t0,2323
  sw $t0,0($t1)
  move $a0,$s2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $s1,0($s2)
  li $t0,2
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t1,$s1,$t0
  li $t0,2323
  sw $t0,0($t1)
  move $v0,$zero
  j L49
  nop
L49:

  lw $s0,16($sp)
  lw $s1,20($sp)
  lw $s2,24($sp)
  lw $s3,28($sp)
  lw $s4,32($sp)
  lw $s5,36($sp)
  lw $s6,40($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop
