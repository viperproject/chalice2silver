import java.io.FileWriter

val vars = 4
var start = 9

val rng = 0 until vars
val ins = rng map ("v" + _)
val outs = rng map ("r" + _)

val insStr = ins.mkString("", ": int, ", ": int")
val outsStr = outs.mkString("", ": int, ", ": int")

val reqUStr = ins.mkString("", " <= " + start + " && ", " <= " + start)
val reqLStr = ins.mkString("", " >= 0 && ", " >= 0")
val ensStr = outs.mkString("", " == 0 && ", " == 0")

var bodyStr = assembleBlock("    ", ins, outs, start)

val classStr = """
class LudicrouslyNestedIfThenElse {
  method foo(%s) returns (%s)
		requires %s
		requires %s
		ensures %s
	{
%s
	}
}""".format(insStr, outsStr, reqUStr, reqLStr, ensStr, bodyStr)

val fw = new FileWriter("ifthenelse_ludicrously_nested.chalice")
fw.write(classStr)
fw.close()

exit(0)

/* 
 * Methods
 */

def assembleBlock(ident: String, ins: Seq[String], outs: Seq[String],
		start: Int): String = {

	var block = ""
	
	assert(ins.size == outs.size, "Requires as many output as input variables.")
	
	if (ins.nonEmpty) {
		val innerBlock =
			if (ins.tail.nonEmpty)
				"\n" + assembleBlock(ident + "  ", ins.tail, outs.tail, start)
			else
				""

		block += ident + "if (%s == %s) {\n".format(ins.head, start)
		block += ident + "  %s := %s - %s\n".format(outs.head, ins.head, start)
		block += innerBlock

		if (start > 0) {		
			for (i <- (0 until start).reverse) {
				block += ident + "} else if (%s == %s) {\n".format(ins.head, i)
				block += ident + "  %s := %s - %s\n".format(outs.head, ins.head, i)
				block += innerBlock
			}
		}
		
		block += ident + "}\n"
	}
	
	block
}