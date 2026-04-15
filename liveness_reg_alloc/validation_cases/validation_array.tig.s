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
tigermain:
addiu $sp, $sp, -48
sw $ra, 44($sp)
sw $fp, 40($sp)
sw $s0, 36($sp)
sw $s1, 32($sp)
sw $s2, 28($sp)
sw $s3, 24($sp)
sw $s4, 20($sp)
sw $s5, 16($sp)
sw $s6, 12($sp)
sw $s7, 8($sp)
addiu $fp, $sp, 48
sw $a0, 0($fp)
li $a0, 3
sw $a0, 0($sp)
li $a1, 5
sw $a1, 4($sp)
jal initArray
li $t2, 0
li $t3, 4
mul $t2, $t2, $t3
add $t2, $t0, $t2
lw $t2, 0($t2)
li $t3, 1
li $t4, 4
mul $t3, $t3, $t4
add $t3, $t0, $t3
lw $t3, 0($t3)
add $t2, $t2, $t3
li $t3, 2
li $t4, 4
mul $t3, $t3, $t4
add $t0, $t0, $t3
lw $t0, 0($t0)
add $t0, $t2, $t0
j L13
L13:
move $sp, $fp
addiu $sp, $sp, -48
lw $ra, 44($sp)
lw $fp, 40($sp)
lw $s0, 36($sp)
lw $s1, 32($sp)
lw $s2, 28($sp)
lw $s3, 24($sp)
lw $s4, 20($sp)
lw $s5, 16($sp)
lw $s6, 12($sp)
lw $s7, 8($sp)
addiu $sp, $sp, 48
jr $ra
nop
