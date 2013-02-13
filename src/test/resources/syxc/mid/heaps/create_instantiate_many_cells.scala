import java.io.FileWriter

val cells = 20 /* Stack overflow ahead! */

val outs = 0 until cells map ("c" + _)
val outsStr = outs.mkString("", ": Cell, ", ": Cell")
val ensStr = outs.zipWithIndex map { case (c, i) =>
	"%s != null && acc(%s.x) && %s.x == %s".format(c, c, c, i)
} mkString ("    ensures ", "\n    ensures ", "")

var bodyStr = outs.zipWithIndex map { case (c, i) =>"""
	  %s := new Cell
	  call %s.set(%s)
""".format(c, c, i)
} mkString ("")

val classStr = """
class InstantiateManyCells {
  method test() returns (%s)
%s
	{%s  }
}

class Cell {
  var x: int
  
  method set(v: int)
    requires acc(x)
    ensures acc(x) && x == v
  { x := v }
}
""".format(outsStr, ensStr, bodyStr)

val fw = new FileWriter("instantiate_many_cells.chalice")
fw.write(classStr)
fw.close()

exit(0)