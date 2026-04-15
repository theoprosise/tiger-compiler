.text
.globl main
.globl tigermain
main:
jal tigermain
move $a0, $v0
li $v0, 1
syscall
li $v0, 10
syscall
L9:
addiu $sp, $sp, -92
sw $ra, 88($sp)
sw $fp, 84($sp)
sw $s0, 80($sp)
sw $s1, 76($sp)
sw $s2, 72($sp)
sw $s3, 68($sp)
sw $s4, 64($sp)
sw $s5, 60($sp)
sw $s6, 56($sp)
sw $s7, 52($sp)
addiu $fp, $sp, 92
sw $a0, 0($fp)
move $t3, $a1
move $t4, $a2
move $t5, $a3
lw $t0, 16($fp)
move $t6, $t0
lw $t0, 20($fp)
move $t7, $t0
lw $t0, 24($fp)
move $t8, $t0
lw $t0, 28($fp)
move $t9, $t0
lw $t0, 32($fp)
sw $t0, -92($fp)
add $t0, $t3, $t4
sw $t0, -88($fp)
add $t0, $t5, $t6
sw $t0, -84($fp)
add $t0, $t7, $t8
sw $t0, -76($fp)
lw $t0, -76($fp)
move $t1, $t0
sw $t1, -80($fp)
lw $t0, -92($fp)
add $t0, $t9, $t0
sw $t0, -68($fp)
lw $t0, -68($fp)
move $t1, $t0
sw $t1, -72($fp)
lw $t0, -88($fp)
lw $t1, -84($fp)
add $t0, $t0, $t1
sw $t0, -60($fp)
lw $t0, -60($fp)
move $t1, $t0
sw $t1, -64($fp)
lw $t0, -80($fp)
lw $t1, -72($fp)
add $t2, $t0, $t1
sw $t2, -52($fp)
lw $t0, -52($fp)
move $t1, $t0
sw $t1, -56($fp)
lw $t0, -64($fp)
lw $t1, -56($fp)
add $t2, $t0, $t1
sw $t2, -44($fp)
lw $t0, -44($fp)
move $t1, $t0
sw $t1, -48($fp)
lw $t0, -48($fp)
add $t0, $t0, $t3
add $t0, $t0, $t5
add $t0, $t0, $t7
add $t0, $t0, $t9
add $t0, $t0, $t4
add $t0, $t0, $t6
add $t1, $t0, $t8
lw $t0, -92($fp)
add $t1, $t1, $t0
lw $t0, -88($fp)
add $t1, $t1, $t0
lw $t0, -84($fp)
add $t1, $t1, $t0
lw $t0, -80($fp)
add $t1, $t1, $t0
lw $t0, -72($fp)
add $v0, $t1, $t0
j L10
L10:
move $sp, $fp
addiu $sp, $sp, -92
lw $ra, 88($sp)
lw $fp, 84($sp)
lw $s0, 80($sp)
lw $s1, 76($sp)
lw $s2, 72($sp)
lw $s3, 68($sp)
lw $s4, 64($sp)
lw $s5, 60($sp)
lw $s6, 56($sp)
lw $s7, 52($sp)
addiu $sp, $sp, 92
jr $ra
nop
tigermain:
addiu $sp, $sp, -76
sw $ra, 72($sp)
sw $fp, 68($sp)
sw $s0, 64($sp)
sw $s1, 60($sp)
sw $s2, 56($sp)
sw $s3, 52($sp)
sw $s4, 48($sp)
sw $s5, 44($sp)
sw $s6, 40($sp)
sw $s7, 36($sp)
addiu $fp, $sp, 76
sw $a0, 0($fp)
sw $fp, 0($sp)
move $a0, $fp
li $a1, 1
sw $a1, 4($sp)
li $a2, 2
sw $a2, 8($sp)
li $a3, 3
sw $a3, 12($sp)
li $t0, 4
sw $t0, 16($sp)
li $t0, 5
sw $t0, 20($sp)
li $t0, 6
sw $t0, 24($sp)
li $t0, 7
sw $t0, 28($sp)
li $t0, 8
sw $t0, 32($sp)
jal L9
j L11
L11:
move $sp, $fp
addiu $sp, $sp, -76
lw $ra, 72($sp)
lw $fp, 68($sp)
lw $s0, 64($sp)
lw $s1, 60($sp)
lw $s2, 56($sp)
lw $s3, 52($sp)
lw $s4, 48($sp)
lw $s5, 44($sp)
lw $s6, 40($sp)
lw $s7, 36($sp)
addiu $sp, $sp, 76
jr $ra
nop
