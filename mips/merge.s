  .globl L148
  .data
  .align 2
  .size L148, 4
L148:
  .word 1
  .ascii " \000"
  .globl L141
  .data
  .align 2
  .size L141, 4
L141:
  .word 1
  .ascii "\012\000"
  .globl L130
  .data
  .align 2
  .size L130, 4
L130:
  .word 1
  .ascii "0\000"
  .globl L119
  .data
  .align 2
  .size L119, 4
L119:
  .word 1
  .ascii "-\000"
  .globl L110
  .data
  .align 2
  .size L110, 4
L110:
  .word 1
  .ascii "0\000"
  .globl L56
  .data
  .align 2
  .size L56, 4
L56:
  .word 1
  .ascii "0\000"
  .globl L26
  .data
  .align 2
  .size L26, 4
L26:
  .word 1
  .ascii "\012\000"
  .globl L24
  .data
  .align 2
  .size L24, 4
L24:
  .word 1
  .ascii " \000"
  .globl L13
  .data
  .align 2
  .size L13, 4
L13:
  .word 1
  .ascii "9\000"
  .globl L6
  .data
  .align 2
  .size L6, 4
L6:
  .word 1
  .ascii "0\000"
  .text
  .align 2
  .globl _tigermain_0_0
_tigermain_0_0:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L180:
  jal getstr
  nop
  move $t0,$v0
  addi $t1,$fp,-4
  sw $t0,0($t1)
  move $t0,$fp
  move $a0,$t0
  sw $a0,0($sp)
  jal readlist_22_2
  nop
  move $t0,$v0
  move $s0,$t0
  jal getstr
  nop
  move $t0,$v0
  addi $t1,$fp,-4
  sw $t0,0($t1)
  move $t0,$fp
  move $a0,$t0
  sw $a0,0($sp)
  jal readlist_22_2
  nop
  move $t0,$v0
  move $t1,$s0
  move $t2,$fp
  move $a0,$t2
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal merge_30_2
  nop
  move $t0,$v0
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal printlist_46_2
  nop
  move $v0,$zero
  j L179
  nop
L179:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl printlist_46_2
printlist_46_2:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L182:
  beqz $a1,L155
  nop
L156:
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
  jal printint_37_2
  nop
  la $t0,L148
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
  lw $t0,4($a1)
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal printlist_46_2
  nop
  move $t0,$zero
L157:
  move $v0,$t0
  j L181
  nop
L155:
  la $t0,L141
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
  j L157
  nop
L181:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl printint_37_2
printint_37_2:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L184:
  bltz $a1,L137
  nop
L138:
  bgtz $a1,L133
  nop
L134:
  la $t0,L130
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
L135:
L139:
  move $v0,$t0
  j L183
  nop
L137:
  la $t0,L119
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  lw $a1,4($fp)
  move $t0,$a1
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal f_38_8
  nop
  move $t0,$zero
  j L139
  nop
L133:
  move $t0,$a1
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal f_38_8
  nop
  move $t0,$zero
  j L135
  nop
L183:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl f_38_8
f_38_8:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L187:
  bgtz $a1,L117
  nop
L118:
  move $v0,$zero
  j L186
  nop
L117:
  li $t0,10
  div $t0,$a1,$t0
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal f_38_8
  nop
  lw $a1,4($fp)
  li $t1,10
  li $t0,10
  div $t0,$a1,$t0
  mul $t0,$t1,$t0
  sub $s0,$a1,$t0
  la $t0,L110
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  move $t0,$v0
  add $t0,$s0,$t0
  move $a0,$t0
  sw $a0,0($sp)
  jal chr
  nop
  move $t0,$v0
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  j L118
  nop
L186:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl merge_30_2
merge_30_2:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
  sw $s1,20($sp)
L189:
  beqz $a1,L102
  nop
L103:
  beqz $a2,L98
  nop
L99:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a2,8($fp)
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $a2,8($fp)
  lw $t1,0($a1)
  lw $t0,0($a2)
  blt $t1,$t0,L94
  nop
L95:
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $a2,8($fp)
  lw $t0,0($a2)
  move $s0,$t0
  move $s1,$a1
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a2,8($fp)
  lw $t1,4($a2)
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$s1
  sw $a1,4($sp)
  move $a2,$t1
  sw $a2,8($sp)
  jal merge_30_2
  nop
  move $t0,$v0
  li $t1,2
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  move $t0,$v0
L96:
L100:
L104:
  move $v0,$t0
  j L188
  nop
L102:
  move $t0,$a2
  j L104
  nop
L98:
  move $t0,$a1
  j L100
  nop
L94:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  move $s0,$t0
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $a2,8($fp)
  lw $t1,4($a1)
  move $t2,$a2
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  move $a2,$t2
  sw $a2,8($sp)
  jal merge_30_2
  nop
  move $t0,$v0
  li $t1,2
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  move $t0,$v0
  j L96
  nop
L188:

  lw $s0,16($sp)
  lw $s1,20($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl readlist_22_2
readlist_22_2:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
  sw $s1,20($sp)
L191:
  move $t0,$zero
  li $t1,1
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal _allocRecord
  nop
  move $s1,$v0
  move $t1,$s1
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  jal readint_5_2
  nop
  move $t0,$v0
  move $s0,$t0
  move $a0,$s1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $t0,0($s1)
  bnez $t0,L76
  nop
L77:
  move $t0,$zero
L78:
  move $v0,$t0
  j L190
  nop
L76:
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  jal readlist_22_2
  nop
  move $t0,$v0
  li $t1,2
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal _allocRecord
  nop
  move $t0,$v0
  j L78
  nop
L190:

  lw $s0,16($sp)
  lw $s1,20($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl readint_5_2
readint_5_2:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
  sw $s1,20($sp)
L197:
  move $s1,$zero
  move $t0,$fp
  move $a0,$t0
  sw $a0,0($sp)
  jal skipto_9_8
  nop
  lw $a1,4($fp)
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  move $s0,$a1
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal isdigit_7_8
  nop
  move $t0,$v0
  sw $t0,0($s0)
L61:
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  move $t1,$fp
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$t0
  sw $a1,4($sp)
  jal isdigit_7_8
  nop
  move $t0,$v0
  bnez $t0,L62
  nop
L52:
  move $v0,$s1
  j L196
  nop
L62:
  li $t0,10
  mul $t0,$t0,$s1
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  li $t0,10
  mul $s0,$t0,$s1
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,-4($t0)
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  move $t0,$v0
  add $t0,$s0,$t0
  move $s0,$t0
  la $t0,L56
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  move $t0,$v0
  sub $t0,$s0,$t0
  move $s1,$t0
  move $t0,$fp
  lw $t0,0($t0)
  addi $t0,$t0,-4
  move $s0,$t0
  jal getstr
  nop
  move $t0,$v0
  sw $t0,0($s0)
  j L61
  nop
L196:

  lw $s0,16($sp)
  lw $s1,20($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl skipto_9_8
skipto_9_8:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L37:
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,0($t0)
  lw $t0,-4($t0)
  move $a0,$t0
  sw $a0,0($sp)
  la $t0,L24
  move $a1,$t0
  sw $a1,4($sp)
  jal _stringCompare
  nop
  move $t0,$v0
  beqz $t0,L30
  nop
L31:
  li $t0,1
  move $s0,$t0
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,0($t0)
  lw $t0,-4($t0)
  move $a0,$t0
  sw $a0,0($sp)
  la $t0,L26
  move $a1,$t0
  sw $a1,4($sp)
  jal _stringCompare
  nop
  move $t0,$v0
  beqz $t0,L28
  nop
L29:
  move $s0,$zero
L28:
  move $t0,$s0
L32:
  bnez $t0,L38
  nop
L34:
  move $v0,$zero
  j L201
  nop
L30:
  li $t0,1
  j L32
  nop
L38:
  move $t0,$fp
  lw $t0,0($t0)
  lw $t0,0($t0)
  addi $t0,$t0,-4
  move $s0,$t0
  jal getstr
  nop
  move $t0,$v0
  sw $t0,0($s0)
  j L37
  nop
L201:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl isdigit_7_8
isdigit_7_8:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
  sw $s1,20($sp)
L205:
  la $t0,L6
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  la $t0,L6
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  lw $a1,4($fp)
  move $t0,$v0
  move $s0,$t0
  move $t0,$a1
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  lw $a1,4($fp)
  move $t0,$v0
  ble $s0,$t0,L19
  nop
L20:
  move $t0,$zero
L21:
  move $v0,$t0
  j L204
  nop
L19:
  li $t0,1
  move $s1,$t0
  move $t0,$a1
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  lw $a1,4($fp)
  move $t0,$a1
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  move $t0,$v0
  move $s0,$t0
  la $t0,L13
  move $a0,$t0
  sw $a0,0($sp)
  jal ord
  nop
  move $t0,$v0
  ble $s0,$t0,L17
  nop
L18:
  move $s1,$zero
L17:
  move $t0,$s1
  j L21
  nop
L204:

  lw $s0,16($sp)
  lw $s1,20($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
