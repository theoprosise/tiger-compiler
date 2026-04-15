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
.data
.align 2
L14:
.word 2
.byte 104
.byte 105
.align 2
.text
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
la $t0, L14
li $v0, 0
j L15
L15:
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
