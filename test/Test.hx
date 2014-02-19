class Test extends haxe.unit.TestCase {
	
	static function main() {
		var r = new haxe.unit.TestRunner();
		r.add(new Test());
		r.run();
	}
	
	function testConst() {
		eeq("1");
		eeq("1.0");
		eeq('"foo"');
		eeq('foo');
		eeq('~/.*/gimsu');
	}
	
	function testHex() {
		eeq("0xabcdef");
		eeq("0xABCDEF");
		eeq("0x0123456789");
	}
	
	function testFloat() {
		eeq(".1");
		eeq("13e12");
		eeq("13e-12");
		eeq("13e+12");
		eeq("13E12");
		eeq("13E-12");
		eeq("13E+12");
		eeq("13.e12");
		eeq("13.14e12");
		eeq("13.14e-12");
		eeq("13.14e+12");
		eeq("13.14E12");
		eeq("13.14E+12");
		eeq("13.14E-12");
	}
	
	function testArrayAccess() {
		eeq("a[1]");
		eeq("a[1][2]");
		eeq("a[1][b[2]][3]");
	}
	
	function testBinops() {
		eeq("1 + 1");
		eeq("1 - 1");
		eeq("1 * 1");
		eeq("1 / 1");
		eeq("1 = 1");
		eeq("1 == 1");
		eeq("1 != 1");
		eeq("1 > 1");
		eeq("1 >= 1");
		eeq("1 < 1");
		eeq("1 <= 1");
		eeq("1 & 1");
		eeq("1 | 1");
		eeq("1 ^ 1");
		eeq("1 && 1");
		eeq("1 || 1");
		eeq("1 << 1");
		eeq("1 >> 1");
		eeq("1 >>> 1");
		eeq("1 % 1");
		eeq("1 ... 1");
		eeq("1 => 1");
	}
	
	function testAssignOps() {
		eeq("1 += 1");
		eeq("1 -= 1");
		eeq("1 *= 1");
		eeq("1 /= 1");
		eeq("1 %= 1");
		eeq("1 &= 1");
		eeq("1 |= 1");
		eeq("1 ^= 1");
		eeq("1 <<= 1");
		eeq("1 >>= 1");
		eeq("1 >>>= 1");
	}
	
	function testFieldAccess() {
		eeq("a.b.c");
		eeq("a.b().c()");
	}
	
	function testParenthesis() {
		eeq("(1)");
		eeq("((1))");
	}
	
	function testObjectDecl() {
		eeq("{ foo : bar }");
		eeq('{ a : 1, b : 2 }');
		eeq('{ a : 1, b : 2, c : 3 }');
	}
	
	function testArrayDecl() {
		eeq("[]");
		eeq("[1]");
		eeq("[1, 2]");
		eeq("[1, 2, 3]");
	}
	
	function testCall() {
		eeq("a()");
		eeq("a(b)");
		eeq("a(b, c)");
	}
	
	function testNew() {
		eeq("new A()");
		eeq("new A(a)");
		eeq("new A(a, b, c)");
		eeq("new A<S, T>(a, b, c)");
	}
	
	function testUnops() {
		eeq("++1");
		eeq("1++");
		eeq("--1");
		eeq("1--");
		eeq("!1");
		eeq("-a");
		eeq("-1");
		eeq("~1");
	}
		
	static function parseExpr(inputCode:String, ?p:haxe.PosInfos) {
		var parser = new haxeparser.HaxeParser(byte.ByteData.ofString(inputCode), '${p.methodName}:${p.lineNumber}');
		var expr = parser.expr();
		return haxe.macro.ExprTools.toString(expr);
	}
	
	function eeq(inputCode:String, ?p:haxe.PosInfos) {
		var inputParsed = parseExpr(inputCode, p);
		assertEquals(inputCode, inputParsed, p);
	}
}