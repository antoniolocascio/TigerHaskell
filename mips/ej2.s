  .globl L155
  .data
  .align 2
  .size L155, 4
L155:
  .word 5
  .ascii "Mal!\012\000"
  .globl L152
  .data
  .align 2
  .size L152, 4
L152:
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
L163:
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
  jal isin_24_5
  nop
  move $t0,$v0
  bnez $t0,L158
  nop
L159:
  la $t0,L155
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
L160:
  move $v0,$t0
  j L162
  nop
L158:
  la $t0,L152
  move $a0,$t0
  sw $a0,0($sp)
  jal print
  nop
  move $t0,$zero
  j L160
  nop
L162:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl printlist_48_5
printlist_48_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L165:
  beqz $a1,L132
  nop
L133:
  move $a0,$a1
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $t0,0($a1)
  move $a0,$t0
  sw $a0,0($sp)
  jal print_int
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
  jal printlist_48_5
  nop
  move $t0,$v0
L134:
  move $v0,$t0
  j L164
  nop
L132:
  move $t0,$zero
  j L134
  nop
L164:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl reverse_43_5
reverse_43_5:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L167:
  beqz $a1,L122
  nop
L123:
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
  lw $t1,4($a1)
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$t1
  sw $a1,4($sp)
  jal reverse_43_5
  nop
  move $t0,$v0
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal snoc_15_5
  nop
  move $t0,$v0
L124:
  move $v0,$t0
  j L166
  nop
L122:
  move $t0,$a1
  j L124
  nop
L166:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl removeall_36_5
removeall_36_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
  sw $s1,20($sp)
L169:
  beqz $a2,L109
  nop
L110:
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $a2,8($fp)
  lw $t0,0($a2)
  beq $a1,$t0,L105
  nop
L106:
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
  jal removeall_36_5
  nop
  move $t0,$v0
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal cons_3_5
  nop
  move $t0,$v0
L107:
L111:
  move $v0,$t0
  j L168
  nop
L109:
  move $t0,$a2
  j L111
  nop
L105:
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
  jal removeall_36_5
  nop
  move $t0,$v0
  j L107
  nop
L168:

  lw $s0,16($sp)
  lw $s1,20($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl remove_29_5
remove_29_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
  sw $s1,20($sp)
L171:
  beqz $a2,L86
  nop
L87:
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $a2,8($fp)
  lw $t0,0($a2)
  beq $a1,$t0,L82
  nop
L83:
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
  jal remove_29_5
  nop
  move $t0,$v0
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal cons_3_5
  nop
  move $t0,$v0
L84:
L88:
  move $v0,$t0
  j L170
  nop
L86:
  move $t0,$a2
  j L88
  nop
L82:
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a2,8($fp)
  lw $t0,4($a2)
  j L84
  nop
L170:

  lw $s0,16($sp)
  lw $s1,20($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl isin_24_5
isin_24_5:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L173:
  beqz $a2,L68
  nop
L69:
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $a2,8($fp)
  lw $t0,0($a2)
  beq $a1,$t0,L64
  nop
L65:
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
  jal isin_24_5
  nop
  move $t0,$v0
L66:
L70:
  move $v0,$t0
  j L172
  nop
L68:
  move $t0,$zero
  j L70
  nop
L64:
  li $t0,1
  j L66
  nop
L172:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl filtra_17_5
filtra_17_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
  sw $s1,20($sp)
L175:
  beqz $a2,L55
  nop
L56:
  move $a0,$a2
  sw $a0,0($sp)
  jal _checkNil
  nop
  lw $a1,4($fp)
  lw $a2,8($fp)
  lw $t0,0($a2)
  beq $t0,$a1,L51
  nop
L52:
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
  jal filtra_17_5
  nop
  move $t0,$v0
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal cons_3_5
  nop
  move $t0,$v0
L53:
L57:
  move $v0,$t0
  j L174
  nop
L55:
  move $t0,$a2
  j L57
  nop
L51:
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
  jal filtra_17_5
  nop
  move $t0,$v0
  j L53
  nop
L174:

  lw $s0,16($sp)
  lw $s1,20($sp)
  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop

  .text
  .align 2
  .globl snoc_15_5
snoc_15_5:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L177:
  move $s0,$a2
  move $t2,$a1
  move $t1,$zero
  lw $t0,0($fp)
  move $a0,$t0
  sw $a0,0($sp)
  move $a1,$t2
  sw $a1,4($sp)
  move $a2,$t1
  sw $a2,8($sp)
  jal cons_3_5
  nop
  move $t0,$v0
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal concat_10_5
  nop
  move $t0,$v0
  move $v0,$t0
  j L176
  nop
L176:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl concat_10_5
concat_10_5:
  sw $fp,-12($sp)
  sw $ra,-8($sp)
  move $fp,$sp
  addi $sp,$sp,-32
  sw $s0,16($sp)
L179:
  beqz $a1,L22
  nop
L23:
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
  jal concat_10_5
  nop
  move $t0,$v0
  lw $t1,0($fp)
  move $a0,$t1
  sw $a0,0($sp)
  move $a1,$s0
  sw $a1,4($sp)
  move $a2,$t0
  sw $a2,8($sp)
  jal cons_3_5
  nop
  move $t0,$v0
L24:
  move $v0,$t0
  j L178
  nop
L22:
  move $t0,$a2
  j L24
  nop
L178:

  lw $s0,16($sp)
  move $sp,$fp
  lw $fp,-12($sp)
  lw $ra,-8($sp)
  j $ra
  nop

  .text
  .align 2
  .globl length_5_5
length_5_5:
  sw $fp,-8($sp)
  sw $ra,-4($sp)
  move $fp,$sp
  addi $sp,$sp,-24
L181:
  beqz $a1,L8
  nop
L9:
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
  jal length_5_5
  nop
  move $t0,$v0
  addi $t0,$t0,1
L10:
  move $v0,$t0
  j L180
  nop
L8:
  move $t0,$zero
  j L10
  nop
L180:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
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
L183:
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
  j L182
  nop
L182:

  move $sp,$fp
  lw $fp,-8($sp)
  lw $ra,-4($sp)
  j $ra
  nop
