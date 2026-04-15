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
addiu $sp, $sp, -40
sw $ra, 36($sp)
sw $fp, 32($sp)
sw $s0, 28($sp)
sw $s1, 24($sp)
sw $s2, 20($sp)
sw $s3, 16($sp)
sw $s4, 12($sp)
sw $s5, 8($sp)
sw $s6, 4($sp)
sw $s7, 0($sp)
addiu $fp, $sp, 40
sw $a0, 0($fp)
li $t0, 0
li $v0, 0
L6:
li $t2, 4
blt $t0, $t2, L7
j L5
L5:
j L8
L7:
add $v0, $v0, $t0
addi $t0, $t0, 1
j L6
L8:
move $sp, $fp
addiu $sp, $sp, -40
lw $ra, 36($sp)
lw $fp, 32($sp)
lw $s0, 28($sp)
lw $s1, 24($sp)
lw $s2, 20($sp)
lw $s3, 16($sp)
lw $s4, 12($sp)
lw $s5, 8($sp)
lw $s6, 4($sp)
lw $s7, 0($sp)
addiu $sp, $sp, 40
jr $ra
nop
