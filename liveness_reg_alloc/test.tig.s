L2:
add $t0, $t6, $t6
add $t1, $t6, $t6
add $t2, $t6, $t6
add $t3, $t6, $t6
add $t4, $t0, $t1
add $t5, $t2, $t3
add $t4, $t4, $t5
add $t4, $t4, $t6
add $t4, $t4, $t6
add $t4, $t4, $t6
add $t4, $t4, $t6
add $t4, $t4, $t6
add $t4, $t4, $t6
add $t4, $t4, $t6
add $t4, $t4, $t6
add $t0, $t4, $t0
add $t0, $t0, $t1
add $t0, $t0, $t2
add $v0, $t0, $t3
j L1
L1:
L4:
move $a0, $fp
li $a1, 1
li $a2, 2
li $a3, 3
li $t0, 4
sw $t0, 0($sp)
li $t0, 5
sw $t0, 4($sp)
li $t0, 6
sw $t0, 8($sp)
li $t0, 7
sw $t0, 12($sp)
li $t0, 8
sw $t0, 16($sp)
jal L0
move $a0, $v0
jal printi
j L3
L3:
