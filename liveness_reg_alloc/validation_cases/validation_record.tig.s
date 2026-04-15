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
addiu $sp, $sp, -44
sw $ra, 40($sp)
sw $fp, 36($sp)
sw $s0, 32($sp)
sw $s1, 28($sp)
sw $s2, 24($sp)
sw $s3, 20($sp)
sw $s4, 16($sp)
sw $s5, 12($sp)
sw $s6, 8($sp)
sw $s7, 4($sp)
addiu $fp, $sp, 44
sw $a0, 0($fp)
li $a0, 8
sw $a0, 0($sp)
jal malloc
li $t2, 1
sw $t2, 0($t0)
li $t2, 2
sw $t2, 4($t0)
lw $t2, 0($t0)
lw $t0, 4($t0)
add $t0, $t2, $t0
j L12
L12:
move $sp, $fp
addiu $sp, $sp, -44
lw $ra, 40($sp)
lw $fp, 36($sp)
lw $s0, 32($sp)
lw $s1, 28($sp)
lw $s2, 24($sp)
lw $s3, 20($sp)
lw $s4, 16($sp)
lw $s5, 12($sp)
lw $s6, 8($sp)
lw $s7, 4($sp)
addiu $sp, $sp, 44
jr $ra
nop
