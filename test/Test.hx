class Test extends haxe.unit.TestCase {
	
	static var whitespaceEreg = ~/[\t\n\r]*/g;
	
	static function main() {
		var r = new haxe.unit.TestRunner();
		r.add(new Test());
		#if haxe_std_path
		r.add(new TestStd());
		#end
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
	
	function testFor() {
		eeq("for (a) b");
	}
	
	function testIn() {
		eeq("a in b");
	}
	
	function testIf() {
		eeq("if (a) b");
		eeq("if (a) b else c");
		eeq("if (a) b else if (c) d else e");
		eeq("if (a) b; else c", "if (a) b else c");
		eeq("if (a) b; else if (c) d; else e", "if (a) b else if (c) d else e");
	}
	
	function testWhile() {
		eeq("while (a) b");
		eeq("do a while (b)");
	}
	
	function testSwitch() {
		eeq("switch (a) {}");
		eeq("switch (a) {case 1:{2;};}");
		eeq("switch (a) {case 1:{2;};case 3:{4;};}");
		eeq("switch (a) {case 1:{2;};case 3:{4;};case 5:{6;};}");
		eeq("switch (a) {case 1:{2;};default:}");
		eeq("switch (a) {case 1 if (2):{2;};}");
		eeq("switch (a) {case _ if (2):{2;};}");
		eeq("switch (a) {default:}");
		eeq("switch (a) {default:{1;};}");
	}
	
	function testTry() {
		eeq("try a catch(b:C) d");
		eeq("try a catch(b:C) d catch(e:F) g");
	}
	
	function testReturn() {
		eeq("return");
		eeq("return x");
	}
	
	function testBreak() {
		eeq("break");
	}
	
	function testContinue() {
		eeq("continue");
	}
	
	function testUntyped() {
		eeq("untyped a");
	}
	
	function testThrow() {
		eeq("throw a");
	}
	
	function testCast() {
		eeq("cast a");
		eeq("cast(a, B)");
	}
	
	function testTernary() {
		eeq("a ? b : c");
		eeq("a ? b : c ? d : e");
	}
	
	function testMeta() {
		eeq("@:meta a");
		eeq("@:meta(a) b");
		eeq("@:meta(a, b) c");
	}
	
	function testPackage() {
		assertEquals(0, parseFile("package;").pack.length);
		assertEquals(1, parseFile("package x;").pack.length);
		assertEquals(1, parseFile(
		    "//test\n"
		  + "package x;"
		).pack.length);
	}
	
	function testClass() {
		peq("class C {}");
		peq("class C extends D {}");
		peq("class C extends p.D {}");
		peq("class C implements p.A {}");
		peq("class C implements A implements B {}");
		peq("class C extends A implements B implements C {}");
		peq("class C<A> {}");
		peq("class C<A, B> {}");
		peq("class C<A:(Int), B:(Float, String)> {}");
	}
	
	function testInterface() {
		peq("interface I {}");
		peq("interface I extends p.D {}");
	}
	
	function testClassField() {
		peq("class C {var a;}");
		peq("class C {function f();}");
		peq("class C {function f(a);}");
		peq("class C {function f(a, b);}");
		peq("class C {var a;var b;}");
		peq("class C {var a(default, null);}");
	}
	
	function testEnum() {
		peq("enum E {}");
		peq("enum E<A> {}");
		peq("enum E<A, B> {}");
		peq("enum E<A:(Int), B:(Float, String)> {}");
	}
	
	function testEnumField() {
		peq("enum E {A;B;C;}");
		peq("enum E {A:Int;B;C:Float;}");
		peq("enum E {A(a:Int);}");
		peq("enum E {A(a:Int, b:String, c:Float);}");
		peq("enum E {A(a:Int, b:String, c:Float);B:Int;}");
		peq("enum E {A(a:Int, b:String, c:Float):E<T>;B:Int;}");
	}
	
	function testAbstract() {
		peq("abstract A {}");
		peq("abstract A(B) {}");
		peq("abstract A(B) from C {}");
		peq("abstract A(B) from C from D {}");
		peq("abstract A(B) from C from D to E {}");
		peq("abstract A(B) from C from D to E to F {}");
		peq("abstract A<S, T>(B<S>) from C<T> from D to E to F {}");
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
		var data = parser.parse();
		return data;
	}
	
	function eeq(inputCode:String, ?expectedCode:String, ?p:haxe.PosInfos) {
		var inputParsed = parseExpr(inputCode, p);
		if (expectedCode == null) {
			expectedCode = inputCode;
		}
		assertEquals(whitespaceEreg.replace(expectedCode, ""), whitespaceEreg.replace(inputParsed, ""), p);
	}
	
	function peq(inputCode:String, ?expectedCode:String, ?p:haxe.PosInfos) {
		var data = parseFile(inputCode, p);
		var decls = data.decls.map(DefinitionConverter.convertTypeDef.bind(data.pack));
		var printer = new haxe.macro.Printer("");
		var reprs = decls.map(printer.printTypeDefinition.bind(_, false));
		var inputParsed = reprs.join("");
		if (expectedCode == null) {
			expectedCode = inputCode;
		}
		assertEquals(whitespaceEreg.replace(expectedCode, ""), whitespaceEreg.replace(inputParsed, ""), p);
	}
}