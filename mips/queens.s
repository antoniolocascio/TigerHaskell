  .globl L48
  .data
  .align 2
  .size L48, 4
L48:
  .word 1
  .ascii "\012\000"
  .globl L42
  .data
  .align 2
  .size L42, 4
L42:
  .word 1
  .ascii "\012\000"
  .globl L32
  .data
  .align 2
  .size L32, 4
L32:
  .word 2
  .ascii " .\000"
  .globl L31
  .data
  .align 2
  .size L31, 4
L31:
  .word 2
  .ascii " O\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-32($sp)
  sw $ra,-28($sp)
  move $fp,$sp
  addi $sp,$sp,-48
L123:
  li $t0,5
  addi $t1,$fp,-4
  sw $t0,0($t1)
  lw $t0,-4($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  addi $t1,$fp,-8
  sw $t0,0($t1)
  lw $t0,-4($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  addi $t1,$fp,-12
  sw $t0,0($t1)
  lw $t0,-4($fp)
  addi $t1,$t0,-1
  lw $t0,-4($fp)
  add $t0,$t1,$t0
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  addi $t1,$fp,-16
  sw $t0,0($t1)
  lw $t0,-4($fp)
  addi $t1,$t0,-1
  lw $t0,-4($fp)
  add $t0,$t1,$t0
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$zero
  sw $a1,4($sp)
  jal _initArray
  nop
  move $t0,$v0
  addi $t1,$fp,-20
  sw $t0,0($t1)
  move $t0,$zero
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal try_21_3
  nop
  move $v0,$zero
  j L122
  nop
L122:

  move $sp,$fp
  lw $fp,-32($sp)
  lw $ra,-28($sp)
  j $ra
  nop

  .text
  .align 2
  .globl try_21_3
try_21_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-40
  sw $s0,16($sp)
  sw $s1,20($sp)
  sw $s2,24($sp)
  sw $s3,28($sp)
L130:
  move $t1,$a1
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  beq $t1,$t0,L114
  nop
L115:
  move $s3,$zero
L113:
  move $t1,$s3
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  addi $t0,$t0,-1
  bgt $t1,$t0,L112
  nop
L111:
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-20($t0)
  move $s1,$t0
  move $t1,$s3
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  addi $t0,$t0,-1
  add $t0,$t1,$t0
  sub $t0,$t0,$a1
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s0,2
  add $t0,$s1,$t0
  lw $t0,0($t0)
  beqz $t0,L68
  nop
L69:
  move $t0,$zero
L70:
  bnez $t0,L78
  nop
L79:
  move $t0,$zero
L80:
  bnez $t0,L109
  nop
L110:
  addi $t0,$s3,1
  move $s3,$t0
  j L113
  nop
L114:
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  jal printboard_14_3
  nop
  move $t0,$zero
L116:
  move $v0,$t0
  j L129
  nop
L68:
  li $t0,1
  move $s0,$t0
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-16($t0)
  move $s2,$t0
  add $t0,$s3,$a1
  move $s1,$t0
  move $a0,$s2
  sw $a0,0($sp)
  move $a1,$s1
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s1,2
  add $t0,$s2,$t0
  lw $t0,0($t0)
  beqz $t0,L66
  nop
L67:
  move $s0,$zero
L66:
  move $t0,$s0
  j L70
  nop
L78:
  li $t0,1
  move $s0,$t0
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-8($t0)
  move $s2,$t0
  move $s1,$s3
  move $a0,$s2
  sw $a0,0($sp)
  move $a1,$s1
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s1,2
  add $t0,$s2,$t0
  lw $t0,0($t0)
  beqz $t0,L76
  nop
L77:
  move $s0,$zero
L76:
  move $t0,$s0
  j L80
  nop
L109:
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-8($t0)
  move $s1,$t0
  move $s0,$s3
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s0,2
  add $t1,$s1,$t0
  li $t0,1
  sw $t0,0($t1)
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-16($t0)
  move $s1,$t0
  add $t0,$s3,$a1
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s0,2
  add $t1,$s1,$t0
  li $t0,1
  sw $t0,0($t1)
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-20($t0)
  move $s1,$t0
  move $t1,$s3
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  addi $t0,$t0,-1
  add $t0,$t1,$t0
  sub $t0,$t0,$a1
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s0,2
  add $t1,$s1,$t0
  li $t0,1
  sw $t0,0($t1)
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-12($t0)
  move $s1,$t0
  move $s0,$a1
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s0,2
  add $t0,$s1,$t0
  sw $s3,0($t0)
  addi $t1,$a1,1
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  jal try_21_3
  nop
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-8($t0)
  move $s1,$t0
  move $s0,$s3
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s0,2
  add $t0,$s1,$t0
  sw $zero,0($t0)
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-16($t0)
  move $s1,$t0
  add $t0,$s3,$a1
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s0,2
  add $t0,$s1,$t0
  sw $zero,0($t0)
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-20($t0)
  move $s1,$t0
  move $t1,$s3
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  addi $t0,$t0,-1
  add $t0,$t1,$t0
  sub $t0,$t0,$a1
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  lw $a1,4($fp)
  sll $t0,$s0,2
  add $t0,$s1,$t0
  sw $zero,0($t0)
  j L110
  nop
L112:
  move $t0,$zero
  j L116
  nop
L129:

  lw $s0,16($sp)
  lw $s1,20($sp)
  lw $s2,24($sp)
  lw $s3,28($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl printboard_14_3
printboard_14_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-40
  sw $s0,16($sp)
  sw $s1,20($sp)
  sw $s2,24($sp)
  sw $s3,28($sp)
L134:
  move $s3,$zero
L47:
  move $t1,$s3
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  addi $t0,$t0,-1
  bgt $t1,$t0,L46
  nop
L45:
  move $s2,$zero
L41:
  move $t1,$s2
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  addi $t0,$t0,-1
  bgt $t1,$t0,L40
  nop
L39:
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-12($t0)
  move $s1,$t0
  move $s0,$s3
  move $a0,$s1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  jal _checkIndexArray
  nop
  sll $t0,$s0,2
  add $t0,$s1,$t0
  lw $t0,0($t0)
  beq $t0,$s2,L33
  nop
L34:
  la $t0,L32
L35:
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  addi $t0,$s2,1
  move $s2,$t0
  j L41
  nop
L33:
  la $t0,L31
  j L35
  nop
L40:
  la $t0,L42
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  addi $t0,$s3,1
  move $s3,$t0
  j L47
  nop
L46:
  la $t0,L48
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $v0,$zero
  j L133
  nop
L133:

  lw $s0,16($sp)
  lw $s1,20($sp)
  lw $s2,24($sp)
  lw $s3,28($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
