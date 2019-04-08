  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L150:
  move $t1,$zero
  li $t0,2
  li $t2,2
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  move $t1,$v0
  move $t0,$zero
  li $t2,2
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  move $t1,$v0
  li $t0,1
  li $t2,2
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  move $t0,$v0
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal isBin_10_3
  nop
  move $t0,$v0
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
  nop
  move $v0,$zero
  j L149
  nop
L149:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl printChildrenPost_34_3
printChildrenPost_34_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L152:
  beqz $a1,L132
  nop
L133:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal printPostOrder_29_3
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,4($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal printChildrenPost_34_3
  nop
  move $t0,$v0
L134:
  move $v0,$t0
  j L151
  nop
L132:
  move $t0,$zero
  j L134
  nop
L151:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl printPostOrder_29_3
printPostOrder_29_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L154:
  beqz $a1,L120
  nop
L121:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal printChildrenPost_34_3
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,4($a1)
  move $a0,$t0
  sw $a0,0($sp)
  jal chr
  nop
  move $t0,$v0
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
L122:
  move $v0,$t0
  j L153
  nop
L120:
  move $t0,$zero
  j L122
  nop
L153:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl printInOrder_20_3
printInOrder_20_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L156:
  beqz $a1,L108
  nop
L109:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  bnez $t0,L104
  nop
L105:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,4($a1)
  move $a0,$t0
  sw $a0,0($sp)
  jal chr
  nop
  move $t0,$v0
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
L106:
L110:
  move $v0,$t0
  j L155
  nop
L108:
  move $t0,$zero
  j L110
  nop
L104:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  move $a0,$t0
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t0,0($t0)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal printInOrder_20_3
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,4($a1)
  move $a0,$t0
  sw $a0,0($sp)
  jal chr
  nop
  move $t0,$v0
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  move $a0,$t0
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t0,4($t0)
  bnez $t0,L98
  nop
L99:
  move $t0,$zero
  j L106
  nop
L98:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  move $a0,$t0
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t0,4($t0)
  move $a0,$t0
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  move $a0,$t0
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t0,4($t0)
  lw $t0,0($t0)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal printInOrder_20_3
  nop
  j L99
  nop
L155:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl isBCompleteRec_17_3
isBCompleteRec_17_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L158:
  beqz $a1,L82
  nop
L83:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal isBComplete_15_3
  nop
  lw $a1,4($fp)
  move $t0,$v0
  bnez $t0,L78
  nop
L79:
  move $t0,$zero
L80:
L84:
  move $v0,$t0
  j L157
  nop
L82:
  li $t0,1
  j L84
  nop
L78:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,4($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal isBCompleteRec_17_3
  nop
  move $t0,$v0
  j L80
  nop
L157:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl isBComplete_15_3
isBComplete_15_3:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L160:
  beqz $a1,L50
  nop
L51:
  li $t0,1
  move $s0,$t0
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t1,0($a1)
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  jal length_5_3
  nop
  lw $a1,4($fp)
  move $t0,$v0
  beqz $t0,L48
  nop
L49:
  move $s0,$zero
L48:
  move $t0,$s0
L52:
  bnez $t0,L66
  nop
L67:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal length_5_3
  nop
  lw $a1,4($fp)
  move $t0,$v0
  li $t1,2
  beq $t0,$t1,L62
  nop
L63:
  move $t0,$zero
L64:
L68:
  move $v0,$t0
  j L159
  nop
L50:
  li $t0,1
  j L52
  nop
L66:
  li $t0,1
  j L68
  nop
L62:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal isBCompleteRec_17_3
  nop
  move $t0,$v0
  j L64
  nop
L159:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl isBinRec_12_3
isBinRec_12_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L162:
  beqz $a1,L39
  nop
L40:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal isBin_10_3
  nop
  lw $a1,4($fp)
  move $t0,$v0
  bnez $t0,L35
  nop
L36:
  move $t0,$zero
L37:
L41:
  move $v0,$t0
  j L161
  nop
L39:
  li $t0,1
  j L41
  nop
L35:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,4($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal isBinRec_12_3
  nop
  move $t0,$v0
  j L37
  nop
L161:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl isBin_10_3
isBin_10_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L164:
  beqz $a1,L23
  nop
L24:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal length_5_3
  nop
  lw $a1,4($fp)
  move $t0,$v0
  li $t1,3
  blt $t0,$t1,L19
  nop
L20:
  move $t0,$zero
L21:
L25:
  move $v0,$t0
  j L163
  nop
L23:
  li $t0,1
  j L25
  nop
L19:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal isBinRec_12_3
  nop
  move $t0,$v0
  j L21
  nop
L163:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl length_5_3
length_5_3:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L166:
  beqz $a1,L7
  nop
L8:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,4($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal length_5_3
  nop
  move $t0,$v0
  addi $t0,$t0,1
L9:
  move $v0,$t0
  j L165
  nop
L7:
  move $t0,$zero
  j L9
  nop
L165:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
