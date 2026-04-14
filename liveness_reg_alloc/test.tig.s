
# ---- liveness ----
n0
  def    = {}
  use    = {}
  in     = {t131 t125}
  out    = {t131 t125}
  ismove = false
n1
  def    = {t132}
  use    = {t125}
  in     = {t125 t131}
  out    = {t131 t132}
  ismove = false
n2
  def    = {t133}
  use    = {t132}
  in     = {t132 t131}
  out    = {t131 t133}
  ismove = false
n3
  def    = {t134}
  use    = {t131 t133}
  in     = {t133 t131}
  out    = {t134}
  ismove = false
n4
  def    = {t101}
  use    = {t134}
  in     = {t134}
  out    = {}
  ismove = true
n5
  def    = {}
  use    = {}
  in     = {}
  out    = {}
  ismove = false
n6
  def    = {}
  use    = {}
  in     = {}
  out    = {}
  ismove = false
# ---- assembly ----
L3:
lw t132, 0(t125)
lw t133, 4(t132)
add t134, t131, t133
move t101, t134
j L2
L2:

# ---- liveness ----
n0
  def    = {}
  use    = {}
  in     = {t124 t125}
  out    = {t124 t125}
  ismove = false
n1
  def    = {t102}
  use    = {t125}
  in     = {t125 t124}
  out    = {t102 t124}
  ismove = true
n2
  def    = {t135}
  use    = {}
  in     = {t124 t102}
  out    = {t124 t102 t135}
  ismove = false
n3
  def    = {t103}
  use    = {t135}
  in     = {t135 t102 t124}
  out    = {t102 t103 t124}
  ismove = true
n4
  def    = {t136}
  use    = {}
  in     = {t124 t103 t102}
  out    = {t124 t103 t102 t136}
  ismove = false
n5
  def    = {t104}
  use    = {t136}
  in     = {t136 t102 t103 t124}
  out    = {t102 t103 t104 t124}
  ismove = true
n6
  def    = {t137}
  use    = {}
  in     = {t124 t104 t103 t102}
  out    = {t124 t104 t103 t102 t137}
  ismove = false
n7
  def    = {t105}
  use    = {t137}
  in     = {t137 t102 t103 t104 t124}
  out    = {t102 t103 t104 t105 t124}
  ismove = true
n8
  def    = {t138}
  use    = {}
  in     = {t124 t105 t104 t103 t102}
  out    = {t124 t105 t104 t103 t102 t138}
  ismove = false
n9
  def    = {}
  use    = {t138 t124}
  in     = {t138 t102 t103 t104 t105 t124}
  out    = {t102 t103 t104 t105 t124}
  ismove = false
n10
  def    = {t139}
  use    = {}
  in     = {t124 t105 t104 t103 t102}
  out    = {t124 t105 t104 t103 t102 t139}
  ismove = false
n11
  def    = {}
  use    = {t139 t124}
  in     = {t139 t102 t103 t104 t105 t124}
  out    = {t102 t103 t104 t105 t124}
  ismove = false
n12
  def    = {t101 t126 t106 t107 t108 t109 t110 t111 t112 t113 t114 t115}
  use    = {t102 t103 t104 t105 t124}
  in     = {t124 t105 t104 t103 t102}
  out    = {}
  ismove = false
n13
  def    = {}
  use    = {}
  in     = {}
  out    = {}
  ismove = false
n14
  def    = {}
  use    = {}
  in     = {}
  out    = {}
  ismove = false
# ---- assembly ----
L5:
move t102, t125
li t135, 1
move t103, t135
li t136, 2
move t104, t136
li t137, 3
move t105, t137
li t138, 4
sw t138, 0(t124)
li t139, 50
sw t139, 4(t124)
jal L1
j L4
L4:

# ---- liveness ----
n0
  def    = {}
  use    = {}
  in     = {t125}
  out    = {t125}
  ismove = false
n1
  def    = {t102}
  use    = {t125}
  in     = {t125}
  out    = {t102}
  ismove = true
n2
  def    = {t140}
  use    = {}
  in     = {t102}
  out    = {t102 t140}
  ismove = false
n3
  def    = {t103}
  use    = {t140}
  in     = {t140 t102}
  out    = {t102 t103}
  ismove = true
n4
  def    = {t101 t126 t106 t107 t108 t109 t110 t111 t112 t113 t114 t115}
  use    = {t102 t103}
  in     = {t103 t102}
  out    = {}
  ismove = false
n5
  def    = {}
  use    = {}
  in     = {}
  out    = {}
  ismove = false
n6
  def    = {}
  use    = {}
  in     = {}
  out    = {}
  ismove = false
# ---- assembly ----
L7:
move t102, t125
li t140, 10
move t103, t140
jal L0
j L6
L6:
