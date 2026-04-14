L10:
add $t2, $t0, $t1
move $t5, $t2
add $t2, $t1, $t1
move $t4, $t2
add $t2, $t1, $t1
move $t3, $t2
add $t2, $t1, $t1
move $t2, $t2
add $t6, $t5, $t4
move $t7, $t6
add $t6, $t3, $t2
move $t6, $t6
add $t6, $t7, $t6
move $t6, $t6
add $t0, $t6, $t0
add $t0, $t0, $t1
add $t0, $t0, $t1
add $t0, $t0, $t1
move $t0, $t0
add $t0, $t0, $t1
add $t0, $t0, $t1
add $t0, $t0, $t1
add $t0, $t0, $t1
move $t0, $t0
add $t0, $t0, $t5
add $t0, $t0, $t4
add $t0, $t0, $t3
add $t0, $t0, $t2
move $t0, $t0
move $t0, $t0
j L9
L9:
L12:
move $t5, $t0
li $t0, 1
move $t4, $t0
li $t0, 2
move $t3, $t0
li $t0, 3
move $t2, $t0
li $t0, 4
sw $t0, 0($t1)
li $t0, 5
sw $t0, 4($t1)
li $t0, 6
sw $t0, 8($t1)
li $t0, 7
sw $t0, 12($t1)
li $t0, 8
sw $t0, 16($t1)
jal L8
move $t0, $t0
move $t5, $t0
jal printi
j L11
L11:
