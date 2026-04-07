L31:
lw t205, 0(t125)
lw t206, 4(t205)
add t207, t204, t206
move t101, t207
j L30
L30:
L33:
move t102, t125
li t208, 1
move t103, t208
li t209, 2
move t104, t209
li t210, 3
move t105, t210
li t211, 4
sw t211, 0(t124)
li t212, 50
sw t212, 4(t124)
jal L29
j L32
L32:
L35:
move t102, t125
li t213, 10
move t103, t213
jal L28
j L34
L34:
