import haxe.CallStack;
import utest.Assert;
import utest.ITest;

class Test implements ITest {

	static var whitespaceEreg = ~/[\t\n\r]*/g;

	var defines:Map<String, String>;

	public function new() {
		defines = new Map<String, String>();
	}

	function setup() {
		defines.clear();
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
		eeq("1 ?? 1");
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
		eeq("1 &&= 1");
		eeq("1 ||= 1");
		eeq("1 ??= 1");
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
		paeq("a in b in c", "(a in (b in c))");
		paeq("a % b in c", "(a % (b in c))");
		paeq("a * b in c", "(a * (b in c))");
		paeq("a / b in c", "(a / (b in c))");
		paeq("a + b in c", "(a + (b in c))");
		paeq("a - b in c", "(a - (b in c))");
		paeq("a << b in c", "(a << (b in c))");
		paeq("a >> b in c", "(a >> (b in c))");
		paeq("a >>> b in c", "(a >>> (b in c))");
		paeq("a | b in c", "(a | (b in c))");
		paeq("a & b in c", "(a & (b in c))");
		paeq("a ^ b in c", "(a ^ (b in c))");
		paeq("a == b in c", "(a == (b in c))");
		paeq("a != b in c", "(a != (b in c))");
		paeq("a > b in c", "(a > (b in c))");
		paeq("a >= b in c", "(a >= (b in c))");
		paeq("a < b in c", "(a < (b in c))");
		paeq("a <= b in c", "(a <= (b in c))");
		paeq("a...b in c", "(a ... (b in c))");
		paeq("a || b in c", "(a || (b in c))");
		paeq("a && b in c", "(a && (b in c))");
		paeq("a => b in c", "(a => (b in c))");
		paeq("a = b in c", "(a = (b in c))");
		paeq("a += b in c", "(a += (b in c))");
		paeq("a ?? b in c", "(a ?? (b in c))");
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
		eeq("@:meta.meta a");
		eeq("@meta.meta a");
	}

	function testPackage() {
		Assert.equals(0, parseFile("package;").pack.length);
		Assert.equals(1, parseFile("package x;").pack.length);
		Assert.equals(2, parseFile("package haxe.macro;").pack.length);
		Assert.equals(2, parseFile("package haxe.extern;").pack.length);
		Assert.equals(1, parseFile(
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
		peq("abstract class C {}");
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
		peq("class C {abstract function f(a, b);}");
		perr("class C {abstract static function f(a, b);}");
		perr("class C {static abstract function f(a, b);}");
		peq("class C {overload function f(a, b);}");
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
		peq("function arr() return [abstract];");
		peq("function foo():Array<String> {return [abstract + 2, abstract(), abstract.hi()];}");
	}

	function testConditionals() {
		eeq("#if true 1 #else 2 #end", "1");
		eeq("#if false 1 #else 2 #end", "2");
		eeq("#if false 1 #elseif true 2 #end", "2");
		eeq("#if false 1 #elseif false 2 #else 3 #end", "3");
		eeq("#if true #if false 1 #else 2 #end #else 3 #end", "2");
		eeq("#if false 1 #else #if false 2 #else 3 #end #end ", "3");
		eeq("#if target.sys 1 #else 2 #end", "2");
		eeq("#if !target.sys 1 #else 2 #end", "1");
		eeq("#if (target.sys) 1 #else 2 #end", "2");
		eeq("#if target.sys 1 #elseif target.php 2 #else 3 #end", "3");
		eeq("#if target.sys 1 #elseif !target.php 2 #else 3 #end", "2");
		eeq("#if (/* Hello */ debug) 1 #else 2 #end", "2");

		eeq("#if (hl_ver >= version('1.11.0')) 1 #else 2 #end", "2");
		eeq("#if (version('1.11.0') < hl_ver) 1 #else 2 #end", "2");
		eeq("#if (version('1.11.0') < version('1.12.0')) 1 #else 2 #end", "1");
		eeq("#if (version('1.11.0-alpha') == version('1.11.0-alpha')) 1 #else 2 #end", "1");
		defines.set("hl_ver", "1.12.0");
		eeq("#if (hl_ver >= version('1.11.0')) 1 #else 2 #end", "1");
		eeq("#if (hl_ver != version('1.11.0')) 1 #else 2 #end", "1");
		eeq("#if (hl_ver == version('1.12.0')) 1 #else 2 #end", "1");

		perr("#if true class C{}");
		perr("#if false class C{}");
		perr("#if true class C{} #else class C{}");
		perr("#if false class C{} #else class C{}");
		perr("#if false class C{} #elseif false class C{}");
		perr("#if false class C{} #elseif true class C{}");
		perr("#if false class C{} #elseif false class C{} #else");
		perr("#if false class C{} #elseif true class C{} #else");
		perr("#if target..sys 1 #else 2 #end");
		perr("#if !target..sys 1 #else 2 #end");
	}

	function testError() {
		eeq("#if true 1 #else #error #end", "1");
		eeq("#if false #error #else 2 #end", "2");
		eeq("#if true 1 #else #error \"Test failed\" #end", "1");
		eeq("#if false #error \"Test failed\" #else 2 #end", "2");

		var err = perr("#if true #error #else 2 #end");
		Assert.isTrue(Std.isOfType(err, haxeparser.HaxeParser.ParserError));
		Assert.equals(haxeparser.HaxeParser.ParserErrorMsg.Unimplemented, err.msg);
		Assert.equals(9, err.pos.min);
		Assert.equals(15, err.pos.max);

		err = perr("#if false 1 #else #error #end");
		Assert.isTrue(Std.isOfType(err, haxeparser.HaxeParser.ParserError));
		Assert.equals(haxeparser.HaxeParser.ParserErrorMsg.Unimplemented, err.msg);
		Assert.equals(18, err.pos.min);
		Assert.equals(24, err.pos.max);

		err = perr("#if true #error \"Test passed\" #else 2 #end");
		Assert.isTrue(Std.isOfType(err, haxeparser.HaxeParser.ParserError));
		Assert.equals('${haxeparser.HaxeParser.ParserErrorMsg.SharpError("Test passed")}', '${Std.string(err.msg)}');
		Assert.equals(9, err.pos.min);
		Assert.equals(15, err.pos.max);

		err = perr("#if false 1 #else #error \"Test passed\" #end");
		Assert.isTrue(Std.isOfType(err, haxeparser.HaxeParser.ParserError));
		Assert.equals('${haxeparser.HaxeParser.ParserErrorMsg.SharpError("Test passed")}', '${Std.string(err.msg)}');
		Assert.equals(18, err.pos.min);
		Assert.equals(24, err.pos.max);
	}

	function testMacro(){
		var fakePosInfos = {
			fileName: "Macro.hx",
			lineNumber : 0,
			className : "Macro",
			methodName : "main"
		}

		eeq("macro 1;",             '({ expr : EConst(CInt(\"1\")), pos : { file : \"main:0\", min : 6, max : 7 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro a;",             '({ expr : EConst(CIdent("a")), pos : { file : "main:0", min : 6, max : 7 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $a;",            '(a : haxe.macro.Expr)',fakePosInfos);
		eeq("macro ${a};",          '(a : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $e{a};",         '(a : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $a{a};",         '({ expr : EArrayDecl(a), pos : { file : "main:0", min : 8, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $b{a};",         '({ expr : EBlock(a), pos : { file : "main:0", min : 8, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro $i{a};",         '({ expr : EConst(CIdent(a)), pos : { file : "main:0", min : 8, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		//eeq("macro $p{a};",       '',fakePosInfos); // ???
		//eeq("macro $v{a};",       '',fakePosInfos); // ???
		#if (haxe >= version("4.3.0-rc.1"))
		eeq("macro var a;",         '({ expr : EVars([{ name : "a", type : null, expr : null, isFinal : false, isStatic : false, meta : [] }]), pos : { file : "main:0", min : 6, max : 9 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro final a;",       '({ expr : EVars([{ name : "a", type : null, expr : null, isFinal : true, isStatic : false, meta : [] }]), pos : { file : "main:0", min : 6, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro final @meta a;", '({ expr : EVars([{ name : "a", type : null, expr : null, isFinal : true, isStatic : false, meta : [{ name : "meta", params : [], pos : { file : "main:0", min : 12, max : 17 } }] }]), pos : { file : "main:0", min : 6, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro @meta var a;",   '({ expr : EMeta({ name : "meta", params : [], pos : { file : "main:0", min : 6, max : 15 } }, { expr : EVars([{ name : "a", type : null, expr : null, isFinal : false, isStatic : false, meta : [] }]), pos : { file : "main:0", min : 12, max : 15 } }), pos : { file : "main:0", min : 6, max : 15 } } : haxe.macro.Expr)',fakePosInfos);
		#else
		eeq("macro var a;",         '({ expr : EVars([{ name : "a", type : null, expr : null, isFinal : false, meta : [] }]), pos : { file : "main:0", min : 6, max : 9 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro final a;",       '({ expr : EVars([{ name : "a", type : null, expr : null, isFinal : true, meta : [] }]), pos : { file : "main:0", min : 6, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro final @meta a;", '({ expr : EVars([{ name : "a", type : null, expr : null, isFinal : true, meta : [{ name : "meta", params : [], pos : { file : "main:0", min : 12, max : 17 } }] }]), pos : { file : "main:0", min : 6, max : 11 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro @meta var a;",   '({ expr : EMeta({ name : "meta", params : [], pos : { file : "main:0", min : 6, max : 15 } }, { expr : EVars([{ name : "a", type : null, expr : null, isFinal : false, meta : [] }]), pos : { file : "main:0", min : 12, max : 15 } }), pos : { file : "main:0", min : 6, max : 15 } } : haxe.macro.Expr)',fakePosInfos);
		#end
		eeq("macro f();",           '({ expr : ECall({ expr : EConst(CIdent("f")), pos : { file : "main:0", min : 6, max : 7 } }, []), pos : { file : "main:0", min : 6, max : 9 } } : haxe.macro.Expr)',fakePosInfos);
		eeq("macro :Array;",        '(TPath({ pack : [], name : "Array", params : [] }) : haxe.macro.Expr.ComplexType)',fakePosInfos);

		eeq("macro class A{};",     '({ pack : [], name : "A", pos : { file : "main:0", min : 6, max : 15 }, meta : [], params : [], isExtern : false, kind : TDClass(null, [], false, false, false), fields : [] } : haxe.macro.Expr.TypeDefinition)',fakePosInfos);
		eeq("macro class A<T>{};",  '({ pack : [], name : "A", pos : { file : "main:0", min : 6, max : 18 }, meta : [], params : [{ name : "T", params : [], meta : [], constraints : [] }], isExtern : false, kind : TDClass(null, [], false, false, false), fields : [] } : haxe.macro.Expr.TypeDefinition)',fakePosInfos);
		eeq("macro.Utils.hello()");
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
		var k = 5000;
		var s = "#if true 1 #else {";
		for (i in 0 ... k) s+="1;";
		s+= "} #end";
		eeq(s, "1");

		s = "#if true 1 #else ";
		for (i in 0...k) s += "#if true ";
		s += "2";
		for (i in 0...k) s += " #end ";
		s += "#end";
		eeq(s, "1");

		s = "#if false ";
		for (i in 0...k) s += "#if true ";
		s += "1";
		for (i in 0...k) s += " #end ";
		s += "#else 2";
		s += "#end";
		eeq(s, "2");

		s = "#if false 1 ";
		for (i in 0...k) s += "#elseif false 1 ";
		s += "#else 2";
		s += "#end";
		eeq(s, "2");
	}

	function testIssue19() {
		eeq("(null : { a:Int, b:String, c:Bool })", "((null : { var a : Int; var b : String; var c : Bool; }))");
	}

	function testIssue30() {
		peq("@:enum abstract Test(Int) {}", "@:enum abstract Test(Int) {}");
		peq("@enum abstract Test(Int) {}", "@enum abstract Test(Int) {}");
	}

	function testIssue31() {
		peq("typedef TypedefName = {>OneTypedef,>OtherTypedef,}", "typedef TypedefName = {>OneTypedef,>OtherTypedef,};");
	}

	function testIssue32() {
		peq("class C { static function main() '{${printClassRec(c,'',s)}}';}", "class C {static function main() '{${printClassRec(c,\\'\\',s)}}';}");
	}

	function testMultilineStringInterpolation() {
		peq("class C { static function main() '{${\nprintClassRec(c,'',s)\n}}';}", "class C {static function main() '{${\\nprintClassRec(c,\\'\\',s)\\n}}';\n}");
	}

	function testFinalFields() {
		peq("class C { final a:Int = 99; }", "class C {final a : Int = 99;}");
        peq("class C { final static function main():Void {} }", "class C {static final function main():Void { }}");
        peq("class C { final function new() {} }", "class C {final function new() { }}");
		peq("final class C {}", "final class C {}");
	}

	function testEnumAbstract() {
		peq("abstract C(Int) {}", "abstract C(Int) {}");
		peq("abstract C(Int) to String {}", "abstract C(Int) to String {}");
		peq("abstract C(Int) from Int to Float {}", "abstract C(Int) from Int to Float {}");
		peq("enum C {}", "enum C {}");

		peq("@:enum abstract C(Int) {}", "@:enum abstract C(Int) {}");
		peq("enum abstract C(Int) {}", "@:enum abstract C(Int) {}");

		peq("abstract Any(Dynamic) {extern inline static function __cast<T>(value:T):Any return cast value;}",
			"abstract Any(Dynamic) {extern inline static function __cast<T>(value:T):Any return cast value;}");
	}

	function testArrowFunctions() {
		peq("class C { var f = () -> Math.random(); }", "class C {var f = () -> return Math.random();}");
		peq("class C { var f = (i) -> i * i + 2; }", "class C {var f = i -> return i * i + 2;}");
		peq("class C { var f = (i:Int) -> i * i + 2; }", "class C {var f = (i:Int) -> return i * i + 2;}");
		peq("class C { var f = (i:Int, j:Float) -> i * j + 2; }", "class C {var f = (i:Int, j:Float) -> return i * j + 2;}");
		peq("class C { var f:Int -> Int; }", "class C {var f : Int -> Int;}");
		peq("class C { var f:Int -> Float -> Int; }", "class C {var f : (Int, Float) -> Int;}");
		peq("class C { var f:(i:Int) -> Int; }", "class C {var f : (i:Int) -> Int;}");
		peq("class C { var f:(?i:Int) -> Int; }", "class C {var f : (?i:Int) -> Int;}");
		peq("class C { var f:(i:Int, j:Float) -> Int; }", "class C {var f : (i:Int, j:Float) -> Int;}");
		peq("class C { var f:(?i:Int, ?j:Float) -> Int; }", "class C {var f : (?i:Int, ?j:Float) -> Int;}");
	}

	function testInline() {
		peq("class C { function test() { inline test(); } }", "class C {function test() {@:inline test();}}");
		peq("class C { function test() { var p = inline new ParamClass<Int>(1); inline p.test(10); } }", "class C {function test() {var p = @:inline new ParamClass<Int>(1);@:inline p.test(10);}}");
	}

	function testIs() {
		eeq("('' is String)", "('' is String)");
		eeq('("" is String)', '("" is String)');
		eeq("([] is String)", "([] is String)");
		eeq("(cast unit.MyEnum.A is Array)", "(cast unit.MyEnum.A is Array)");
		eeq("(map is haxe.ds.StringMap)", "(map is haxe.ds.StringMap)");
	}

	function testTypeIntersection() {
		eeq("function memberMultiple < A:Base & I1 > (a:A):A { return a; }", "function memberMultiple<A:(Base & I1)>(a:A):A {return a;}");
		eeq("function memberAnon < A:{ x : Int } & { y : Float }> (v:A) { return v.x + v.y; }", "function memberAnon<A:({ var x : Int; } & { var y : Float; })>(v:A) {return v.x + v.y;}");
		peq("private typedef C2 = {} & A;", "typedef C2 = { } & A;");
		peq("private typedef D2 = A & B;", "typedef D2 = A & B;");
	}

	function testMetadataVarDecl() {
		eeq("var @:a(b) c:D = e");
	}

	function testModuleStatics() {
		peq('final finalInit = "finalInit";');
		peq('final finalHintInit : String = "finalHintInit";');
		peq('inline final inlineFinalInit = "inlineFinalInit";');
		peq('inline final inlineFinalHintInit : String = "inlineFinalHintInit";');
		peq('private final privateFinalInit = "privateFinalInit";');
		peq('private final privateFinalHintInit : String = "privateFinalHintInit";');
		peq('private inline final privateInlineFinalInit = "privateInlineFinalInit";');
		peq('private inline final privateInlineFinalHintInit : String = "privateInlineFinalHintInit";');
		peq('inline private final inlinePrivateFinalInit = "inlinePrivateFinalInit";');
		peq('inline private final inlinePrivateFinalHintInit : String = "inlinePrivateFinalHintInit";');
		peq('var varInit = "varInit";');
		peq('var varInitHint : String = "varInitHint";');
		peq('var varHint : String;');
		peq('inline var inlineVarInit = "inlineVarInit";');
		peq('inline var inlineVarInitHint : String = "inlineVarInitHint";');
		peq('private var privateVarInit = "privateVarInit";');
		peq('private var privateVarInitHint : String = "privateVarInitHint";');
		peq('private var privateVarHint : String;');
		peq('private inline var privateInlineVarInit = "privateInlineVarInit";');
		peq('private inline var privateInlineVarInitHint : String = "privateInlineVarInitHint";');
		peq('inline private var inlinePrivateVarInit = "inlinePrivateVarInit";');
		peq('inline private var inlinePrivateVarInitHint : String = "inlinePrivateVarInitHint";');
		peq('function func() return "func";');
		peq('private function privateFunc() return "privateFunc";');
		peq('private inline function privateInlineFunc() return "privateInlineFunc";');
		peq('inline private function inlinePrivateFunc() return "inlinePrivateFunc";');
		peq('dynamic function dynamicFunc() return "dynamicFunc";');
		peq('private dynamic function privateDynamicFunc() return "privateDynamicFunc";');
		peq('dynamic private function dynamicPrivateFunc() return "dynamicPrivateFunc";');
		peq('@:isVar var prop(get, set) : String = "prop";function get_prop() return prop + "-get";function set_prop(value) return prop = value + "-set";');
		peq('overload function func() return "func";');
		peq("function func(...r:Int){}", "function func(r:haxe.Rest<Int>) { }");
		peq("function func(a:Int, ...r:Int){}", "function func(a:Int, r:haxe.Rest<Int>) { }");
		peq("function func(a:Int, ...r:Array<Int>){}", "function func(a:Int, r:haxe.Rest<Array<Int>>) { }");
		peq("function methodWithRest(rest:Rest<Int>):Rest<Int> {return super.methodWithRest(...rest.append(999));}");
	}

	function testimport() {
		try {
			parseFile('import haxe.macro.function.Test;');
			Assert.isTrue(true);
		} catch (e:haxe.Exception) {
			Assert.isTrue(false);
		}
	}

	#if (haxe >= version("4.3.0-rc.1"))
	function testDefaultTypeParams() {
		peq("class DefaultTPClass_y<T=String> {}");
		peq("class DefaultTPClass_yn<S=String, T> {}");
		peq("class DefaultTPClass_ny<S, T=String> {}");
		peq("class DefaultTPClass_yy<S=Int, T=String> {}");
		peq("class DefaultTPClass_yy<S=pack.sub.Type, T=String> {}");
		peq("class DefaultTPClass_yy<S:(pack.sub.Type)=pack.sub.TypeImpl, T=String> {}");
		peq("class DefaultTPClass_yy<S:pack.sub.Type=pack.sub.TypeImpl, T=String> {}", "class DefaultTPClass_yy<S:(pack.sub.Type)=pack.sub.TypeImpl, T=String> {}");
	}

	function testLocalStatic() {
		peq("function basic() {static var x = 1, y;}");
		peq('function basic() {static final y = "final", z = 10;}');
	}
	#end

	function parseExpr(inputCode:String, ?p:haxe.PosInfos) {
		var parser = new haxeparser.HaxeParser(byte.ByteData.ofString(inputCode), '${p.methodName}:${p.lineNumber}');
		for (key => value in defines) parser.define(key, value);
		var expr = parser.expr();
		return haxe.macro.ExprTools.toString(expr);
	}

	function parseFile(inputCode:String, ?p:haxe.PosInfos) {
		var parser = new haxeparser.HaxeParser(byte.ByteData.ofString(inputCode), '${p.methodName}:${p.lineNumber}');
		for (key => value in defines) parser.define(key, value);
		var data = parser.parse();
		return data;
	}

	function eeq(inputCode:String, ?expectedCode:String, ?p:haxe.PosInfos) {
		var inputParsed = parseExpr(inputCode, p);
		if (expectedCode == null) {
			expectedCode = inputCode;
		}
		Assert.equals(whitespaceEreg.replace(expectedCode, ""), whitespaceEreg.replace(inputParsed, ""), p);
	}

	function perr(inputCode:String, ?p:haxe.PosInfos){
		var catchError = false;
		var err:Dynamic = null;
		try {
			parseFile(inputCode);
		}
		catch (e:Dynamic){
			catchError = true;
			err = e;
		}
		Assert.isTrue(catchError);
		return err;
	}

	function peq(inputCode:String, ?expectedCode:String, ?p:haxe.PosInfos) {
		var data = parseFile(inputCode, p);
		var decls = data.decls.map(function(t){return haxeparser.DefinitionConverter.convertTypeDef(data.pack, t.decl);});
		var printer = new haxe.macro.Printer("");
		var reprs = decls.map(printer.printTypeDefinition.bind(_, false));
		var inputParsed = reprs.join("");
		if (expectedCode == null) {
			expectedCode = inputCode;
		}
		Assert.equals(whitespaceEreg.replace(expectedCode, ""), whitespaceEreg.replace(inputParsed, ""), p);
	}

	function parentize(e:haxe.macro.Expr) {
		return switch e.expr {
			case EConst(_): e;
			case _:
				e = haxe.macro.ExprTools.map(e, parentize);
				{expr: haxe.macro.Expr.ExprDef.EParenthesis(e), pos: e.pos};
		}
	}

	function paeq(inputCode:String, ?expectedCode:String, ?p:haxe.PosInfos) {
		var parser = new haxeparser.HaxeParser(byte.ByteData.ofString(inputCode), '${p.methodName}:${p.lineNumber}');
		var expr = parser.expr();
		var printer = new haxe.macro.Printer();
		if (expectedCode == null) {
			expectedCode = inputCode;
		}
		Assert.equals(printer.printExpr(parentize(expr)), expectedCode, p);
	}
}
