class Test extends haxe.unit.TestCase {
	
	static var whitespaceEreg = ~/[\t\n\r]*/g;
	
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
	
	function testVars() {
		eeq("var x");
		eeq("var x = 1");
	}
	
	function testFunction() {
		eeq("function() null");
		eeq("function(x) null");
		eeq("function(x, y) null");
		eeq("function(x = 0, y) null");
		eeq("function(x, y = 0) null");
		eeq("function(x = 0, y = 0) null");
		eeq("function(x:Int, y) null");
		eeq("function(x, y:Int) null");
		eeq("function(x:Int, y:Int) null");
		
		eeq("function f() null");
		eeq("function f(x) null");
		eeq("function f(x, y) null");
		eeq("function f(x = 0, y) null");
		eeq("function f(x, y = 0) null");
		eeq("function f(x = 0, y = 0) null");
		eeq("function f(x:Int, y) null");
		eeq("function f(x, y:Int) null");
		eeq("function f(x:Int, y:Int) null");
	}
	
	function testBlock() {
		eeq("{a;}");
		eeq("{a;b;}");
		eeq("{a;b;c;}");
		eeq("{var x, y;}");
		eeq("{var x = 0, y;}");
		eeq("{var x, y = 0;}");
		eeq("{var x = 0, y = 0;}");
		eeq("{var x:Int, y;}");
		eeq("{var x = 0, y:Int;}");
		eeq("{var x, y:Int = 0;}");
		eeq("{var x:Int = 0, y:Int = 0;}");
	}
	
	function testPackage() {
		assertEquals(0, parseFile("package;").pack.length);
		assertEquals(1, parseFile("package x;").pack.length);
		assertEquals(1, parseFile(
		    "//test\n"
		  + "package x;"
		).pack.length);
	}
	
	function testConditionals() {
		eeq("#if true 1 #else 2 #end", "1");
		eeq("#if false 1 #else 2 #end", "2");
		eeq("#if false 1 #elseif true 2 #end", "2");
		eeq("#if false 1 #elseif false 2 #else 3 #end", "3");
	}
		
	static function parseExpr(inputCode:String, ?p:haxe.PosInfos) {
		var parser = new haxeparser.HaxeParser(byte.ByteData.ofString(inputCode), '${p.methodName}:${p.lineNumber}');
		var expr = parser.expr();
		return haxe.macro.ExprTools.toString(expr);
	}
	
	static function parseFile(inputCode:String, ?p:haxe.PosInfos) {
		var parser = new haxeparser.HaxeParser(byte.ByteData.ofString(inputCode), '${p.methodName}:${p.lineNumber}');
		var expr = parser.parse();
		return expr;
	}
	
	function eeq(inputCode:String, ?expectedCode:String, ?p:haxe.PosInfos) {
		var inputParsed = parseExpr(inputCode, p);
		if (expectedCode == null) {
			expectedCode = inputCode;
		}
		assertEquals(whitespaceEreg.replace(expectedCode, ""), whitespaceEreg.replace(inputParsed, ""), p);
	}
}