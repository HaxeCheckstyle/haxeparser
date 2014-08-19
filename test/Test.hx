class Test extends haxe.unit.TestCase {

	static var whitespaceEreg = ~/[\t\n\r]*/g;

	static function main() {
		//workaround for haxe issue #2902
		var p:hxparse.Parser<hxparse.LexerTokenSource<haxeparser.Data.Token>, haxeparser.Data.Token>;

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
		eeq("#if true #if false 1 #else 2 #end #else 3 #end", "2");
		eeq("#if false 1 #else #if false 2 #else 3 #end #end ", "3");

		perr("#if true class C{}");
		perr("#if false class C{}");
		perr("#if true class C{} #else class C{}");
		perr("#if false class C{} #else class C{}");
		perr("#if false class C{} #elseif false class C{}");
		perr("#if false class C{} #elseif true class C{}");
		perr("#if false class C{} #elseif false class C{} #else");
		perr("#if false class C{} #elseif true class C{} #else");
	}

	function testMacro(){
		var fakePosInfos = {
			fileName: "Macro.hx",
			lineNumber : 0,
			className : "Macro",
			methodName : "main"
		}

		eeq("macro 1;",           '({ expr : EConst(CInt(\"1\")), pos : { file : \"main:0\", min : 6, max : 7 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro a;",           '({ expr : EConst(CIdent("a")), pos : { file : "main:0", min : 6, max : 7 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $a;",          '(a : haxe.macro.Expr)',fakePosInfos);
		eeq("macro ${a};",        '(a : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $e{a};",       '(a : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $a{a};",       '({ expr : EArrayDecl(a), pos : { file : "main:0", min : 8, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $b{a};",       '({ expr : EBlock(a), pos : { file : "main:0", min : 8, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $i{a};",       '({ expr : EConst(CIdent(a)), pos : { file : "main:0", min : 8, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		//eeq("macro $p{a};",       '',fakePosInfos); // ???
		//eeq("macro $v{a};",       '',fakePosInfos); // ???
		eeq("macro var a;",       '({ expr : EVars([{ name : "a", type : null, expr : null }]), pos : { file : "main:0", min : 6, max : 9 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro @meta var a;", '({ expr : EMeta({ name : "meta", params : [], pos : { file : "main:0", min : 7, max : 15 } }, { expr : EVars([{ name : "a", type : null, expr : null }]), pos : { file : "main:0", min : 12, max : 15 } }), pos : { file : "main:0", min : 7, max : 15 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro f();",         '({ expr : ECall({ expr : EConst(CIdent("f")), pos : { file : "main:0", min : 6, max : 7 } }, []), pos : { file : "main:0", min : 6, max : 9 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro :Array;",      '(TPath({ pack : [], name : "Array", params : [] }) : haxe.macro.Expr.ComplexType)',fakePosInfos);
		eeq("macro class A{};",   '({ pack : [], name : "A", pos : { file : "main:0", min : 6, max : 15 }, meta : [], params : [], isExtern : false, kind : TDClass(null, [], false), fields : [] } : haxe.macro.Expr.TypeDefinition)',fakePosInfos);
		eeq("macro class A<T>{};",'({ pack : [], name : "A", pos : { file : "main:0", min : 6, max : 18 }, meta : [], params : [{ name : "T", params : [], constraints : [] }], isExtern : false, kind : TDClass(null, [], false), fields : [] } : haxe.macro.Expr.TypeDefinition)',fakePosInfos);
	}

	function testIssue6() {
		peq("class Test {
			function main() {
				if(true){
					trace(\"ok\");
				}
				//
			}
		}", "class Test {
			function main() {
				if (true) {
					trace(\"ok\");
				};
			}
		}");
	}

	function testIssue7(){
		// stack overflow on large inputs
		var s = "#if true 1 #else {";
		for (i in 0 ... 5000) s+="1;";
		s+= "} #end";
		eeq(s, "1");

		s = "#if true 1 #else ";
		for (i in 0...5000) s += "#if true ";
		s += "2";
		for (i in 0...5000) s += " #end ";
		s += "#end";
		eeq(s, "1");

		s = "#if false ";
		for (i in 0...5000) s += "#if true ";
		s += "1";
		for (i in 0...5000) s += " #end ";
		s += "#else 2";
		s += "#end";
		eeq(s, "2");

		s = "#if false 1 ";
		for (i in 0...5000) s += "#elseif false 1 ";
		s += "#else 2";
		s += "#end";
		eeq(s, "2");
	}

	function testIssue19() {
		eeq("(null : { a:Int, b:String, c:Bool })", "(null : { var a : Int; var b : String; var c : Bool; })");
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

	function perr(inputCode:String, ?p:haxe.PosInfos){
		var catchError = false;
		try {
			parseFile(inputCode);
		}
		catch (e:Dynamic){
			catchError = true;
		}
		assertTrue(catchError);
	}

	function peq(inputCode:String, ?expectedCode:String, ?p:haxe.PosInfos) {
		var data = parseFile(inputCode, p);
		var decls = data.decls.map(function(t){return DefinitionConverter.convertTypeDef(data.pack, t.decl);});
		var printer = new haxe.macro.Printer("");
		var reprs = decls.map(printer.printTypeDefinition.bind(_, false));
		var inputParsed = reprs.join("");
		if (expectedCode == null) {
			expectedCode = inputCode;
		}
		assertEquals(whitespaceEreg.replace(expectedCode, ""), whitespaceEreg.replace(inputParsed, ""), p);
	}
}