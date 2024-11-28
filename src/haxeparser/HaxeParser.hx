package haxeparser;

import haxeparser.Data;
import haxe.macro.Expr;
import haxe.ds.Option;

enum ParserErrorMsg {
	MissingSemicolon;
	MissingType;
	DuplicateDefault;
	UnclosedMacro;
	Unimplemented;
	Custom(s:String);
	SharpError(s:String);
}

class ParserError {
	public var msg: ParserErrorMsg;
	public var pos: Position;
	public function new(message:ParserErrorMsg, pos:Position) {
		this.msg = message;
		this.pos = pos;
	}
}

enum SmallType {
	SNull;
	SBool(b:Bool);
	SFloat(f:Float);
	SString(s:String);
	SVersion(v:Version);
}

typedef Version = {
	major:Int,
	minor:Int,
	patch:Int,
	pre:String
}

class HaxeCondParser extends hxparse.Parser<hxparse.LexerTokenSource<Token>, Token> implements hxparse.ParserBuilder {
	public function new(stream){
		super(stream);
	}

	public function parseMacroCond(allowOp:Bool):{tk:Option<Token>, expr:Expr}
	{
		return switch stream {
			case [{tok:Const(CIdent(t)), pos:p}]:
				parseMacroIdent(allowOp, t, p);
			case [{tok:Const(CString(s, qs)), pos:p}]:
				{tk:None, expr:{expr:EConst(CString(s, qs)), pos:p}};
			case [{tok:Const(CInt(i, s)), pos:p}]:
				{tk:None, expr:{expr:EConst(CInt(i, s)), pos:p}};
			case [{tok:Const(CFloat(f, s)), pos:p}]:
				{tk:None, expr:{expr:EConst(CFloat(f, s)), pos:p}};
			case [{tok:Kwd(k), pos:p}]:
				parseMacroIdent(allowOp, HaxeParser.keywordString(k), p);
			case [{tok:Unop(op), pos:p}, o = parseMacroCond(allowOp)]:
				{tk:o.tk, expr:HaxeParser.makeUnop(op, o.expr, p)};
			case [{tok:POpen, pos:p1}, o = expr(), {tok:PClose, pos:p2}]:
				{ tk:None, expr:{expr:EParenthesis(o), pos:HaxeParser.punion(p1, p2)} };
		}
	}

	public function expr():Expr {
		return switch stream {
			case [{tok:Const(c), pos:p}]: exprNext(mapConstant(c, p));
			case [{tok:Kwd(KwdThis), pos:p}]: exprNext({expr: EConst(CIdent("this")), pos:p});
			case [{tok:Kwd(KwdTrue), pos:p}]: exprNext({expr: EConst(CIdent("true")), pos:p});
			case [{tok:Kwd(KwdFalse), pos:p}]: exprNext({expr: EConst(CIdent("false")), pos:p});
			case [{tok:Kwd(KwdNull), pos:p}]: exprNext({expr: EConst(CIdent("null")), pos:p});
			case [{tok:POpen, pos: p1}]:
				switch stream {
					case [e = expr()]:
						switch stream {
							case [{tok:PClose, pos:p2}]:
								exprNext({expr: EParenthesis(e), pos:HaxeParser.punion(p1, p2)});
							case _: unexpected();
						}
				}
			case [{tok:Unop(op), pos:p1}, e = expr()]: HaxeParser.makeUnop(op,e,p1);
			case _: unexpected();
		}
	}

	function exprNext(e1:Expr):Expr {
		return switch stream {
			case [{tok:Dot, pos:p}, e = parseField(e1, Normal, p)]: e;
			case [{tok:QuestionDot, pos:p}, e = parseField(e1, Safe, p)]: e;
			case [{tok:POpen, pos:_}]:
				switch stream {
					case [params = parseCallParams(), {tok:PClose, pos:p2}]:
						exprNext({expr:ECall(e1,params),pos:HaxeParser.punion(e1.pos,p2)});
					case _: unexpected();
				}
			case [{tok:BkOpen}, e2 = expr(), {tok:BkClose, pos:p2}]:
				exprNext({expr:EArray(e1,e2), pos:HaxeParser.punion(e1.pos,p2)});
			case [{tok:Binop(OpGt)}]:
				switch stream {
					case [{tok:Binop(OpGt)}]:
						switch stream {
							case [{tok:Binop(OpGt)}]:
								switch stream {
									case [{tok:Binop(OpAssign)}, e2 = expr()]:
										HaxeParser.makeBinop(OpAssignOp(OpUShr),e1,e2);
									case [e2 = expr()]: HaxeParser.makeBinop(OpUShr,e1,e2);
								}
							case [{tok:Binop(OpAssign)}, e2 = expr()]:
								HaxeParser.makeBinop(OpAssignOp(OpShr),e1,e2);
							case [e2 = expr()]:
								HaxeParser.makeBinop(OpShr,e1,e2);
						}
					case [{tok:Binop(OpAssign)}]:
						HaxeParser.makeBinop(OpGte,e1, expr());
					case [e2 = expr()]:
						HaxeParser.makeBinop(OpGt,e1,e2);
				}
			case [{tok:Binop(op)}, e2 = expr()]:
				HaxeParser.makeBinop(op,e1,e2);
			case [{tok:Spread}, e2 = expr()]:
				HaxeParser.makeBinop(OpInterval,e1,e2);
			case [{tok:Question}, e2 = expr(), {tok:DblDot}, e3 = expr()]:
				{ expr: ETernary(e1,e2,e3), pos: HaxeParser.punion(e1.pos, e3.pos)};
			case _: e1;
		}
	}

	function parseField(e1, efk, p) {
		return switch stream {
			case [{tok:Kwd(KwdMacro), pos:p2}]:
				exprNext({expr:EField(e1, "macro", efk), pos:HaxeParser.punion(e1.pos, p2)});
			case [{tok:Kwd(KwdExtern), pos:p2}]:
				exprNext({expr:EField(e1, "extern", efk), pos:HaxeParser.punion(e1.pos, p2)});
			case [{tok:Kwd(KwdFunction), pos:p2}]:
				exprNext({expr:EField(e1, "function", efk), pos:HaxeParser.punion(e1.pos, p2)});
			case [{tok:Kwd(KwdNew), pos:p2}]:
				exprNext({expr:EField(e1, "new", efk), pos:HaxeParser.punion(e1.pos, p2)});
			case [{tok:Kwd(k), pos:p2}]:
				exprNext({expr:EField(e1, KeywordPrinter.toString(k), efk), pos:HaxeParser.punion(e1.pos, p2)});
			case [{tok:Const(CIdent(f)), pos:p2}]:
				exprNext({expr:EField(e1, f, efk), pos:HaxeParser.punion(e1.pos, p2)});
			case [{tok:Dollar(v), pos:p2}]:
				exprNext({expr:EField(e1, "$" + v, efk), pos:HaxeParser.punion(e1.pos, p2)});
			case _:
				switch(e1) {
					case {expr: EConst(CInt(v)), pos:p2} if (p2.max == p.min):
						exprNext({expr:EConst(CFloat(v + ".")), pos:HaxeParser.punion(p, p2)});
					case _: unexpected();
				}
		}
	}

	function parseCallParams() {
		var ret = [];
		switch stream {
			case [e = expr()]: ret.push(e);
			case _: return [];
		}
		while(true) {
			switch stream {
				case [{tok: Comma}, e = expr()]: ret.push(e);
				case _: break;
			}
		}
		return ret;
	}

	function parseMacroIdent(allowOp:Bool, t:String, p:Position):{tk:Option<Token>, expr:Expr}
	{
		return {tk:None, expr:{expr:EConst(CIdent(t)), pos:p}};
	}
}
enum abstract SkipState(Int){
	var Consume    = 0;       // consume current branch
	var SkipBranch = 1;       // skip until next #elsif/#else
	var SkipRest   = 2;       // skip until #end
}

class HaxeTokenSource {
	var lexer:HaxeLexer;
	@:allow(haxeparser.HaxeParser)
	var mstack:Array<Position>;
	@:allow(haxeparser.HaxeParser)
	var skipstates:Array<SkipState>;

	var defines:Map<String, Dynamic>;

	var rawSource:hxparse.LexerTokenSource<Token>;
	var condParser:HaxeCondParser;

	public function new(lexer,defines){
		this.lexer = lexer;
		this.mstack = [];
		this.defines = defines;
		this.skipstates = [Consume];
		this.rawSource = new hxparse.LexerTokenSource(lexer,HaxeLexer.sharp_token);
		this.condParser = new HaxeCondParser(this.rawSource);
	}

	function lexerToken() {
		return lexer.token(HaxeLexer.tok);
	}

	inline function getSt() return skipstates[skipstates.length-1];
	inline function setSt(s:SkipState) skipstates[skipstates.length-1] = s;
	inline function pushSt(s:SkipState) skipstates.push(s);
	inline function popSt(){
		return (skipstates.length>1) ? skipstates.pop() : throw('unexpected #end');
	}

	public function token():Token{
		while(true){
			var tk    = lexerToken();
			var state = getSt();
			switch ([tk.tok,state]:Array<Dynamic>) {
				case [CommentLine(_) | Comment(_) | Sharp("line"),_]:
				case [Sharp("error"),Consume]:
					var nextTok = lexerToken();
					switch nextTok.tok {
						case Const(CString(str)):throw new ParserError(SharpError(str), tk.pos);
						case _:throw new ParserError(Unimplemented, tk.pos);
					}
				case [Sharp("if"),Consume]:
					mstack.push(tk.pos);
					pushSt( enterMacro() ? Consume : SkipBranch );
				case [Sharp("if"),SkipBranch|SkipRest]:
					deepSkip(); // alternatively use push_st(SkipRest) here
				case [Sharp("end"),_]:
					mstack.pop();
					popSt();
				case [Sharp("elseif"),Consume]:
					setSt(SkipRest);
				case [Sharp("elseif"),SkipBranch]:
					setSt( enterMacro() ? Consume : SkipBranch );
				case [Sharp("else"),SkipBranch]:
					setSt(Consume);
				case [Sharp("else"),Consume]:
					setSt(SkipRest);
				case [Sharp(_),SkipRest]:
				case [_,Consume]:
					return tk;
				case [Eof,_]:
					return tk;
				case [_,_]:
			}
		}
	}

	inline function enterMacro(){
		var o = condParser.parseMacroCond(false);
		return isTrue(eval(o.expr));
	}

	function deepSkip(){
		var lvl = 1;
		while(true){
			var tk = lexerToken();
			switch tk.tok {
				case Sharp("if"):
					lvl += 1;
				case Sharp("end"):
					lvl -= 1;
					if (lvl == 0)
						return;
				case Eof:
					throw 'unclosed macro';
				case _:
			}
		}
	}

	function isTrue(a:SmallType)
	{
		return switch a {
			case SBool(false), SNull, SFloat(0.0), SString(""): false;
			case _: true;
		}
	}

	function compare(a:SmallType, b:SmallType)
	{
		return switch [a, b] {
			case [SNull, SNull]: 0;
			case [SFloat(a), SFloat(b)]: Reflect.compare(a, b);
			case [SString(a), SString(b)]: Reflect.compare(a, b);
			case [SBool(a), SBool(b)]: Reflect.compare(a, b);
			case [SVersion(a), SVersion(b)]: compareVersion(a, b);
			case [SVersion(a), SString(b)]: compareVersion(a, parseVersion(b));
			case [SString(a), SVersion(b)]: compareVersion(parseVersion(a), b);
			case [SNull, SVersion(b)]: compareVersion(null, b);
			case [SVersion(a), SNull]: compareVersion(a, null);
			case [SString(a), SFloat(b)]: Reflect.compare(Std.parseFloat(a), b);
			case [SFloat(a), SString(b)]: Reflect.compare(a, Std.parseFloat(b));
			case _: 0;
		}
	}

	function compareVersion(a:Version, b:Version) {
		if ((a == null) && (b == null)) return 0;
		if (a == null) return -1;
		if (b == null) return 1;
		if (a.major > b.major) return 1;
		if (a.major < b.major) return -1;
		if (a.minor > b.minor) return 1;
		if (a.minor < b.minor) return -1;
		if (a.patch > b.patch) return 1;
		if (a.patch < b.patch) return -1;
		if (a.pre > b.pre) return 1;
		if (a.pre < b.pre) return -1;
		return 0;
	}

	function eval(e:Expr)
	{
		return switch (e.expr)
		{
			case EConst(CIdent(s)): defines.exists(s) ? SString('${defines.get(s)}') : SNull;
			case EConst(CString(s)): SString(s);
			case EConst(CInt(f)), EConst(CFloat(f)): SFloat(Std.parseFloat(f));
			case ECall({expr: EConst(CIdent('version')), pos: p1},[{expr: EConst(CString(s)), pos: p2}]): SVersion(parseVersion(s));
			case EBinop(OpBoolAnd, e1, e2): SBool(isTrue(eval(e1)) && isTrue(eval(e2)));
			case EBinop(OpBoolOr, e1, e2): SBool(isTrue(eval(e1)) || isTrue(eval(e2)));
			case EUnop(OpNot, _, e): SBool(!isTrue(eval(e)));
			case EParenthesis(e): eval(e);
			case EBinop(op, e1, e2):
				var v1 = eval(e1);
				var v2 = eval(e2);
				var cmp = compare(v1, v2);
				var val = switch (op)
				{
					case OpEq: cmp == 0;
					case OpNotEq: cmp != 0;
					case OpGt: cmp > 0;
					case OpGte: cmp >= 0;
					case OpLt: cmp < 0;
					case OpLte: cmp <= 0;
					case _: throw "Unsupported operation";
				}
				SBool(val);
			case _: throw "Invalid condition expression";
		}
	}

	function parseVersion(s:String):Version{
		var reg = ~/([0-9]+)\.([0-9]+)\.([0-9]+)(.*)/;

		var major:Int = 0;
		var minor:Int = 0;
		var patch:Int = 0;
		var pre:String = "";

		if (reg.match(s)) {
			major = Std.parseInt(reg.matched(1));
			minor = Std.parseInt(reg.matched(2));
			patch = Std.parseInt(reg.matched(3));
			pre = reg.matched(4);
		}
		return { major: major, minor: minor, patch: patch,pre: pre };
	}

	public function curPos():hxparse.Position{
		return lexer.curPos();
	}
}

class HaxeParser extends hxparse.Parser<HaxeTokenSource, Token> implements hxparse.ParserBuilder {

	var defines:Map<String, Dynamic>;

	var doResume = false;
	var doc:String;
	var inMacro:Bool;

	public function new(input:byte.ByteData, sourceName:String) {
		defines = new Map();
		defines.set("true", true);

		var lexer = new HaxeLexer(input, sourceName);
		var ts = new HaxeTokenSource(lexer, defines);
		super(ts);

		inMacro = false;
		doc = "";
	}

	public function define(flag:String, ?value:Dynamic)
	{
		defines.set(flag, value);
	}

	public function parse() {
		var res = parseFile();
		if (stream.mstack.length != 0) throw new ParserError(UnclosedMacro, stream.mstack[stream.mstack.length-1]);
		return res;
	}

	@:allow(haxeparser.HaxeCondParser)
	static function keywordString(k:Keyword)
	{
		return Std.string(k).substr(3).toLowerCase();
	}

	@:allow(haxeparser.HaxeCondParser)
	static function punion(p1:Position, p2:Position) {
		return {
			file: p1.file,
			min: p1.min < p2.min ? p1.min : p2.min,
			max: p1.max > p2.max ? p1.max : p2.max,
		};
	}

	static function quoteIdent(s:String) {
		// TODO
		return s;
	}

	static function isLowerIdent(s:String) {
		function loop(p) {
			var c = s.charCodeAt(p);
			return if (c >= 'a'.code && c <= 'z'.code)
				true
			else if (c == '_'.code) {
				if (p + 1 < s.length)
					loop(p + 1);
				else
					true;
			} else
				false;
		}
		return loop(0);
	}

	static function isPostfix(e:Expr, u:Unop) {
		return switch (u) {
			case OpIncrement | OpDecrement | OpNot:
				switch(e.expr) {
					case EConst(_) | EField(_) | EArray(_):
						true;
					case _:
						false;
				}
			case OpNeg | OpNegBits: false;
			case OpSpread: false;
		}
	}

	static function isPrefix(u:Unop) {
		return switch(u) {
			case OpIncrement | OpDecrement: true;
			case OpNot | OpNeg | OpNegBits: true;
			case OpSpread: true;
		}
	}

	static function precedence(op:Binop) {
		var left = true;
		var right = false;
		return switch(op) {
			case OpIn : {p: 0, left: right};
			case OpMod : {p: 1, left: left};
			case OpMult | OpDiv : {p: 2, left: left};
			case OpAdd | OpSub : {p: 3, left: left};
			case OpShl | OpShr | OpUShr : {p: 4, left: left};
			case OpOr | OpAnd | OpXor : {p: 5, left: left};
			case OpNullCoal: {p: 6, left: left};
			case OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte : {p: 7, left: left};
			case OpInterval : {p: 8, left: left};
			case OpBoolAnd : {p: 9, left: left};
			case OpBoolOr : {p: 10, left: left};
			case OpArrow : {p: 11, left: right};
			case OpAssign | OpAssignOp(_) : {p:12, left:right};
		}
	}

	static function isNotAssign(op:Binop) {
		return switch(op) {
			case OpAssign | OpAssignOp(_): false;
			case _: true;
		}
	}

	static function isDollarIdent(e:Expr) {
		return switch (e.expr) {
			case EConst(CIdent(n)) if (n.charCodeAt(0) == "$".code): true;
			case _: false;
		}
	}

	static function swap(op1:Binop, op2:Binop) {
		var i1 = precedence(op1);
		var i2 = precedence(op2);
		return i1.left && i1.p <= i2.p;
	}

	@:allow(haxeparser.HaxeCondParser)
	static function makeBinop(op:Binop, e:Expr, e2:Expr) {
		return switch (e2.expr) {
			case EBinop(_op,_e,_e2) if (swap(op,_op)):
				var _e = makeBinop(op,e,_e);
				{expr: EBinop(_op,_e,_e2), pos:punion(_e.pos,_e2.pos)};
			case ETernary(e1,e2,e3) if (isNotAssign(op)):
				var e = makeBinop(op,e,e1);
				{expr:ETernary(e,e2,e3), pos:punion(e.pos, e3.pos)};
			case _:
				{ expr: EBinop(op,e,e2), pos:punion(e.pos, e2.pos)};
		}
	}

	@:allow(haxeparser.HaxeCondParser)
	static function makeUnop(op:Unop, e:Expr, p1:Position) {
		function neg(s:String) {
			return s.charCodeAt(0) == '-'.code
				? s.substr(1)
				: "-" + s;
		}
		return switch(e.expr) {
			case EBinop(bop, e, e2):
				{ expr: EBinop(bop, makeUnop(op, e, p1), e2), pos: punion(p1,e.pos)};
			case ETernary(e1, e2, e3):
				{ expr: ETernary(makeUnop(op, e1, p1), e2, e3), pos:punion(p1,e.pos)};
			case EIs(e, t):
				{ expr: EIs(makeUnop(op, e, p1), t), pos:punion(p1, e.pos)};
			case EConst(CInt(v, s)) if (op.match(OpNeg)):
				{ expr: EConst(CInt(neg(v), s)), pos:punion(p1, e.pos)};
			case EConst(CFloat(v, s)) if (op.match(OpNeg)):
				{ expr: EConst(CFloat(neg(v), s)), pos:punion(p1, e.pos)};
			case _:
				{ expr: EUnop(op,false,e), pos:punion(p1,e.pos)};
		}
	}

	static function makeMeta(name:String, params:Array<Expr>, e:Expr, p1:Position) {
		return switch(e.expr) {
			case EBinop(bop,e,e2):
				{ expr: EBinop(bop, makeMeta(name,params,e,p1), e2), pos: punion(p1,e.pos)};
			case ETernary(e1,e2,e3):
				{ expr: ETernary(makeMeta(name,params,e1,p1), e2, e3), pos:punion(p1,e.pos)};
			case _:
				{ expr: EMeta({name:name, params:params, pos:p1}, e), pos: punion(p1, e.pos) };
		}
	}

	static var nullPos:Position = {min: 0, max:0, file:"<null pos>"};

	static function makeIs(e:Expr, t:TypePath, p:Position, p_is:Position) {
		var e_is = {expr: EField({expr:EConst(CIdent("Std")), pos:nullPos}, "is"), pos:p_is};
		var e2 = exprOfTypePath(t.pack, t.name, p);
		return {expr:ECall(e_is, [e, e2]), pos:p};
	}

	static function exprOfTypePath(pack:Array<String>, name:String, p:Position) {
		if (pack.length <= 0) {
			return {expr:EConst(CIdent(name)), pos:p};
		}
		var e = {expr:EConst(CIdent(pack.pop())), pos:p};
		for (pa in pack) {
			e = {expr:EField(e, pa), pos:p};
		}
		return {expr:EField(e, name), pos:p};
	}

	static function apush<T>(a:Array<T>, t:T) {
		a.push(t);
		return a;
	}

	static function aunshift<T>(a:Array<T>, t:T) {
		a.unshift(t);
		return a;
	}

	function psep<T>(sep:TokenDef, f:Void->T):Array<T> {
		var acc = [];
		while(true) {
			try {
				acc.push(f());
				switch stream {
					case [{tok: sep2} && sep2 == sep]:
				}
			} catch(e:hxparse.NoMatch<Dynamic>) {
				break;
			}
		}
		return acc;
	}

	function ident() {
		return switch stream {
			case [{tok:Const(CIdent(i)),pos:p}]: { name: i, pos: p};
		}
	}

	function dollarIdent() {
		return switch stream {
			case [{tok:Const(CIdent(i)),pos:p}]: { name: i, pos: p};
			case [{tok:Dollar(i), pos:p}]: { name: "$" + i, pos: p};
		}
	}

	function dollarIdentMacro(pack:Array<String>) {
		return switch stream {
			case [{tok:Const(CIdent(i)),pos:p}]: { name: i, pos: p};
			case [{tok:Dollar(i), pos:p}]: { name: "$" + i, pos: p};
			case [{tok:Kwd(KwdMacro), pos: p} && pack.length > 0]: { name: "macro", pos: p };
			case [{tok:Kwd(KwdExtern), pos: p} && pack.length > 0]: { name: "extern", pos: p };
			case [{tok:Kwd(KwdFunction), pos: p} && pack.length > 0]: { name: "function", pos: p };
		}
	}

	function lowerIdentOrMacro() {
		return switch stream {
			case [{tok:Const(CIdent(i))} && isLowerIdent(i)]: i;
			case [{tok:Kwd(KwdMacro)}]: "macro";
			case [{tok:Kwd(KwdExtern)}]: "extern";
			case [{tok:Kwd(KwdFunction)}]: "function";
		}
	}

	function anyEnumIdent() {
		return switch stream {
			case [i = ident()]: i;
			case [{tok:Kwd(k), pos:p}]: {name:k.getName().toLowerCase(), pos:p};
		}
	}

	function propertyIdent() {
		return switch stream {
			case [i = ident()]: i.name;
			case [{tok:Kwd(KwdDynamic)}]: "dynamic";
			case [{tok:Kwd(KwdDefault)}]: "default";
			case [{tok:Kwd(KwdNull)}]: "null";
		}
	}

	function questionableDollarIdent() {
		var po = switch stream {
			case [{tok:Question,pos:p}]: p;
			case _: null;
		}
		var ident = dollarIdent();
		return {opt: (po != null), name: ident.name, pos: ident.pos}
	}

	function getDoc() {
		return "";
	}

	function comma() {
		return switch stream {
			case [{tok:Comma}]:
		}
	}

	function semicolon() {
		return if (last.tok == BrClose) {
			switch stream {
				case [{tok: Semicolon, pos:p}]: p;
				case _: last.pos;
			}
		} else switch stream {
			case [{tok: Semicolon, pos:p}]: p;
			case _:
				var pos = last.pos;
				if (doResume)
					pos
				else
					throw new ParserError(MissingSemicolon, pos);
			}
	}

	function parseFile() {
		return switch stream {
			case [{tok:Kwd(KwdPackage)}, p = parsePackage(), _ = semicolon(), l = parseTypeDecls(p,[]), {tok:Eof}]:
				{ pack: p, decls: l };
			case [l = parseTypeDecls([],[]), {tok:Eof}]:
				{ pack: [], decls: l };
		}
	}

	function parseTypeDecls(pack:Array<String>, acc:Array<TypeDecl>) {
		return switch stream {
			case [ v = parseTypeDecl(), l = parseTypeDecls(pack,apush(acc,v)) ]:
				l;
			case _: acc;
		}
	}

	function parseAbstract (doc, meta, flags:Array<{c:ClassFlag, e:EnumFlag, a:haxeparser.Data.AbstractFlag, s:StaticFlag, t:TypedefFlag, pos:Position}>, p1) {
		return switch stream {
			case [name = typeName(), tl = parseConstraintParams(), st = parseAbstractSubtype(), sl = parseRepeat(parseAbstractRelations)]:
				var fl = switch stream {
					case [{tok:BrOpen}, fl = parseClassFields(false, p1)]:
						fl;
				}
				var aflags = flags.map(function(i) return i.a);
				if (st != null) {
					aflags.push(AbOver(st));
				}
				{ decl: EAbstract({
					name: name,
					doc: doc,
					meta: meta,
					params: tl,
					flags: aflags.concat(sl),
					data: fl.fields
				}), pos: punion(p1, fl.pos)};
		}
	}

	function parseClassContent(doc, meta, flags:Array<{c:ClassFlag, e:EnumFlag, a:haxeparser.Data.AbstractFlag, s:StaticFlag, t:TypedefFlag, pos:Position}>, n, p1) {
		var name = typeName();
		var tl = parseConstraintParams();
		var hl = parseRepeat(parseClassHerit);
		var fl = switch stream {
			case [{tok:BrOpen}, fl = parseClassFields(false, p1)]:
				fl;
		}
		return {decl: EClass({
				name: name,
				doc: doc,
				meta: meta,
				params: tl,
				flags: flags.map(function(i) return i.c).concat(n).concat(hl),
				data: fl.fields
			}), pos: punion(p1,fl.pos)};
	}

	function parseTypeDecl() {
		return switch stream {
			case [{tok:Kwd(KwdImport), pos:p1}]:
				parseImport(p1);
			case [{tok:Kwd(KwdUsing), pos:p1}]:
				parseUsing(p1);
			case [doc = getDoc(), meta = parseMeta(), c = parseCommonFlags()]:
				switch stream {
					case [{tok:Kwd(KwdFunction), pos:p1}, name = parseFunName(), pl = parseConstraintParams(), {tok:POpen}, al = psep(Comma, parseFunParam), {tok:PClose}, t = parseTypeOpt()]:
						var e = switch stream {
							case [e = toplevelExpr(), _ = semicolon()]:
								{ expr: e, pos: e.pos };
							case [{tok: Semicolon,pos:p}]:
								{ expr: null, pos: p}
							case _: unexpected();
						}
						var f = {
							params: pl,
							args: al,
							ret: t,
							expr: e.expr
						}
						{decl: EStatic({
							name: name,
							doc: doc,
							meta: meta,
							params: pl,
							flags: c.map(function(i) return i.s),
							data: FFun(f)
						}), pos: punion(p1, e.pos)};
					case [{tok:Kwd(KwdVar), pos:p1}, name = dollarIdent()]:
							switch stream {
								case [{tok:POpen}, i1 = propertyIdent(), {tok:Comma}, i2 = propertyIdent(), {tok:PClose}]:
									var t = parseTypeOpt();
									var e = parseVarFieldAssignment();
									{decl: EStatic({
										name: name.name,
										doc: doc,
										meta: meta,
										params: [],
										flags: c.map(function(i) return i.s),
										data: FProp(i1,i2,t,e.expr)
									}), pos: punion(p1, e.pos)};

								case [t = parseTypeOpt()]:
									var e = parseVarFieldAssignment();
									{decl: EStatic({
										name: name.name,
										doc: doc,
										meta: meta,
										params: [],
										flags: c.map(function(i) return i.s),
										data: FVar(t,e.expr)
									}), pos: punion(p1, e.pos)};
							}
					case [{tok:Kwd(KwdEnum), pos:p1}]:
						switch stream {
							case [{tok:Kwd(KwdAbstract), pos:p1}, a = parseAbstract(doc, meta, c.concat([{{c:null, e:null, a:AbEnum, s:null, t:null, pos:null}}]), p1)]:
								{ decl: a.decl, pos: punion(p1, a.pos)};
							case [name = typeName(), tl = parseConstraintParams(), {tok:BrOpen}, l = parseRepeat(parseEnum), {tok:BrClose, pos: p2}]:
								{decl: EEnum({
									name: name,
									doc: doc,
									meta: meta,
									params: tl,
									flags: c.map(function(i) return i.e),
									data: l
								}), pos: punion(p1,p2)};
						}
					case [flags = parseClassFlags()]:
						parseClassContent(doc, meta, c, flags.flags, flags.pos);
					case [{tok: Kwd(KwdTypedef), pos: p1}, name = typeName(), tl = parseConstraintParams(), {tok:Binop(OpAssign), pos: p2}, t = parseComplexType()]:
						switch stream {
							case [{tok:Semicolon}]:
							case _:
						}
						{ decl: ETypedef({
							name: name,
							doc: doc,
							meta: meta,
							params: tl,
							flags: c.map(function(i) return i.t),
							data: t
						}), pos: punion(p1,p2)};

					case [{tok: Kwd(KwdAbstract), pos:p1}]:
						switch stream {
							case [a = parseAbstract(doc, meta, c, p1)]:
								{decl: a.decl, pos: a.pos};
							case _:
								var c2 = parseCommonFlags();
								switch stream {
									case [flags = parseClassFlags()]:
										parseClassContent(doc, meta, c.concat(c2), apush(flags.flags, HAbstract), p1);
									case _:
								}
						}

					case [name = dollarIdent(), t = parseTypeOpt(), e = parseVarFieldAssignment()]:
						{decl: EStatic({
							name: name.name,
							doc: doc,
							meta: meta,
							params: [],
							flags: c.map(function(i) return i.s),
							data: FVar(t,e.expr)
						}), pos: (c.length > 0) ? punion(c[0].pos, e.pos) : e.pos};
				}
		}
	}

	function parseClass(meta:Metadata, cflags:Array<{fst: ClassFlag, snd:String}>, needName:Bool) {
		var optName = if (needName) typeName else function() {
			var t = parseOptional(typeName);
			return t == null ? "" : t;
		}
		return switch stream {
			case [flags = parseClassFlags(), doc = getDoc(), name = optName(), tl = parseConstraintParams(), hl = psep(Comma,parseClassHerit), {tok: BrOpen}, fl = parseClassFields(false,flags.pos)]:
				{ decl: EClass({
					name: name,
					doc: doc,
					meta: meta,
					params: tl,
					flags: cflags.map(function(i) return i.fst).concat(flags.flags).concat(hl),
					data: fl.fields
				}), pos: punion(flags.pos,fl.pos)};
		}
	}

	function parseImportApostrophe() {
		function matchPart() {
			return switch stream {
				case [{tok:Const(CIdent(k)), pos: p}]:
					{pack:k, pos:p};
				case [{tok:Kwd(KwdMacro), pos:p}]:
					{pack:"macro", pos:p};
				case [{tok:Kwd(KwdExtern), pos:p}]:
					{pack:"extern", pos:p};
				case [{tok:Kwd(KwdFunction), pos:p}]:
					{pack:"function", pos:p};
				case _: unexpected();
			}
		}

		var acc = [matchPart()];
		while(true) {
			switch stream {
				case [{tok: Dot}]:
					switch stream {
						case [{tok:Binop(OpMult)}]:
							return EImport(acc, IAll);
						case _:
					}
					acc.push (matchPart());
				case [{tok:Kwd(KwdIn) | Binop(OpIn) | Const(CIdent('as'))}, {tok:Const(CIdent(name))}]:
					return EImport(acc, IAsName(name));
				case _:
					return EImport (acc, INormal);
			}
		}
		unexpected();
	}

	function parseImport(p1:Position) {
		return switch stream {
			case [decl = parseImportApostrophe(), {tok:Semicolon, pos:p2}]:
				return {
					decl: decl,
					pos: punion (p1, p2)
				}
			case _:
				unexpected();
		}
	}

	function parseUsingApostrophe() {
		function matchPart() {
			return switch stream {
				case [{tok:Const(CIdent(k))}]:
					k;
				case [{tok:Kwd(KwdMacro)}]:
					"macro";
				case [{tok:Kwd(KwdExtern)}]:
					"extern";
				case [{tok:Kwd(KwdFunction)}]:
					"function";
				case _: unexpected();
			}
		}
		var acc = [matchPart()];
		while(true) {
			switch stream {
				case [{tok: Dot}]:
					acc.push(matchPart());
				case _:
					break;
			}
		}

		return acc;
	}

	function parseUsing(p1:Position) {
		return switch stream {
			case [acc = parseUsingApostrophe(), {tok:Semicolon, pos:p2}]:
				var name = acc.pop();
				return {
					decl: EUsing({
						pack: acc,
						name: name
					}),
					pos: punion(p1, p2)
				};
			case _:
				unexpected();
		}
	}

	function parseAbstractRelations():haxeparser.Data.AbstractFlag {
		return switch stream {
			case [{tok:Const(CIdent("to"))}, t = parseComplexType()]: AbTo(t);
			case [{tok:Const(CIdent("from"))}, t = parseComplexType()]: AbFrom(t);
		}
	}

	function parseAbstractSubtype() {
		return switch stream {
			case [{tok:POpen}, t = parseComplexType(), {tok:PClose}]: t;
			case _: null;
		}
	}

	function parsePackage() {
		return psep(Dot, lowerIdentOrMacro);
	}

	function parseClassFields(tdecl:Bool, p1:Position):{fields:Array<Field>, pos:Position} {
		var l = parseClassFieldResume(tdecl);
		var p2 = switch stream {
			case [{tok: BrClose, pos: p2}]:
				p2;
			case _: unexpected();
		}
		return {
			fields: l,
			pos: p2
		}
	}

	function parseClassFieldResume(tdecl:Bool):Array<Field> {
		return parseRepeat(parseClassField);
	}

	function parseCommonFlags():Array<{c:ClassFlag, e:EnumFlag, a:haxeparser.Data.AbstractFlag, s:StaticFlag, t:TypedefFlag, pos:Position}> {
		return switch stream {
			case [{tok:Kwd(KwdPrivate), pos:p}, l = parseCommonFlags()]: aunshift(l, {c:HPrivate, e:EPrivate, a:AbPrivate, s:SPrivate, t:TDPrivate, pos:p});
			case [{tok:Kwd(KwdExtern), pos:p}, l = parseCommonFlags()]: aunshift(l, {c:HExtern, e:EExtern, a:AbExtern, s:null, t:TDExtern, pos:p});
			case [{tok:Kwd(KwdFinal), pos:p}, l = parseCommonFlags()]: aunshift(l, {c:HFinal, e:null, a:null, s:SFinal, t:null, pos:p});
			case [{tok:Kwd(KwdMacro), pos:p}, l = parseCommonFlags()]: aunshift(l, {c:null, e:null, a:null, s:SMacro, t:null, pos:p});
			case [{tok:Kwd(KwdDynamic), pos:p}, l = parseCommonFlags()]: aunshift(l, {c:null, e:null, a:null, s:SDynamic, t:null, pos:p});
			case [{tok:Kwd(KwdInline), pos:p}, l = parseCommonFlags()]: aunshift(l, {c:null, e:null, a:null, s:SInline, t:null, pos:p});
			// included for completeness, there are no Public / Static flags
			// case [{tok:Kwd(KwdPublic), pos:p}, l = parseCommonFlags()]: aunshift(l, {c:null, e:null, a:null, s:null, t:null, pos:p});
			// case [{tok:Kwd(KwdStatic), pos:p}, l = parseCommonFlags()]: aunshift(l, {c:null, e:null, a:null, s:null, t:null, pos:p});
			case [{tok:Kwd(KwdOverload), pos:p}, l = parseCommonFlags()]: aunshift(l, {c:null, e:null, a:null, s:SOverload, t:null, pos:p});
			case _: [];
		}
	}

	function parseMetaParams(pname:Position) {
		return switch stream {
			case [{tok: POpen, pos:p} && p.min == pname.max, params = psep(Comma, expr), {tok: PClose}]: params;
			case _: [];
		}
	}

	function parseMetaEntry():haxe.macro.Expr.MetadataEntry {
		return switch stream {
			case [{tok:At, pos:p1}, name = parseMetaName(p1), params = parseMetaParams(name.pos)]: {name: name.name, params: params, pos: name.pos};
		}
	}

	function parseMeta() {
		return switch stream {
			case [entry = parseMetaEntry()]: apush(parseMeta(), entry);
			case _: [];
		}
	}

	function parseMetaName2(p1, acc:Array<{name:String, pos:Position}>) {
		var part = switch stream {
			case [{tok:Const(CIdent(i)), pos:p}] if (p.miun = p1.max):
				{name:i, pos:punion(p, p1)};
			case [{tok:Kwd(k), pos:p}] if (p.miun = p1.max):
				{name:KeywordPrinter.toString(k), pos:punion(p, p1)};
		}
		acc.unshift(part);
		return switch stream {
			case [{tok:Dot, pos:p1}, part = parseMetaName2(p1, acc)]: part;
			case _: acc;
		}
	}

	function parseMetaName(p1) {
		return switch stream {
			case [{tok:DblDot, pos:p}] if (p.min = p1.max):
				switch stream {
					case [names = parseMetaName2(p, [])]:
						metaNameConcat(names, false);
					case _:
						unexpected();
				}
			case [names = parseMetaName2(p1, [])]:
				metaNameConcat(names, true);
		}
	}

	function metaNameConcat(names:Array<{name:String, pos:Position}>, custom:Bool):{name:String, pos:Position} {
		if (names.length <= 0) {
			unexpected();
		}
		var pos = names[0].pos;
		var nameParts:Array<String> = [];
		for (n in names){
			nameParts.push(n.name);
			pos = punion(pos, n.pos);
		}
		nameParts.reverse();
		if (custom) {
			return {name:nameParts.join("."), pos:pos};
		}
		else {
			return {name:":" + nameParts.join("."), pos:pos};
		}
	}

	function parseEnumFlags() {
		return switch stream {
			case [{tok:Kwd(KwdEnum), pos:p}]: {flags: [], pos: p};
		}
	}

	function parseClassFlags() {
		return switch stream {
			case [{tok:Kwd(KwdClass), pos:p}]: {flags: [], pos: p};
			case [{tok:Kwd(KwdInterface), pos:p}]: {flags: apush([],HInterface), pos: p};
		}
	}

	function parseTypeHint() {
		return switch stream {
			case [{tok: DblDot}, t = parseComplexType()]: t;
		}
	}

	function parseTypeOpt() {
		return switch stream {
			case [t = parseTypeHint()]: t;
			case _: null;
		}
	}

	function parseComplexType() {
		return parseComplexTypeMaybeNamed(false);
	}

	function parseComplexTypeMaybeNamed(allowNamed:Bool) {
		return switch stream {
			case [{tok:POpen, pos:p1}, tl = psep(Comma, function () return parseComplexTypeMaybeNamed(true)), {tok:PClose, pos:p2}]:
				switch tl {
					case []:
						parseFunctionTypeNext(tl, p1);
					case [TNamed(_, _)]:
						parseFunctionTypeNext(tl, p1);
					case [t]:
						var t = TParent(t);
						parseComplexTypeNext(t);
					case _:
						parseFunctionTypeNext(tl, p1);
				}
			case _:
				var t = parseComplexTypeInner(allowNamed);
				parseComplexTypeNext(t);
		}
	}

	function parseFunctionTypeNext(tl, p1) {
		return switch stream {
			case [{tok:Arrow, pos:pa}]:
				switch stream {
					case [tret = parseComplexTypeInner(false)]:
						TFunction(tl, tret);
				}
			case _:
				unexpected();
		}
	}

	function parseStructuralExtension() {
		return switch stream {
			case [{tok: Binop(OpGt)}, t = parseTypePath(), {tok: Comma}]: t;
		}
	}

	function parseComplexTypeInner(allowNamed:Bool):ComplexType {
		return switch stream {
			case [{tok:POpen}, t = parseComplexType(), {tok:PClose}]: TParent(t);
			case [{tok:BrOpen, pos: p1}]:
				switch stream {
					case [l = parseTypeAnonymous(false)]: TAnonymous(l);
					case [t = parseStructuralExtension()]:
						var tl = parseRepeat(parseStructuralExtension);
						tl.unshift(t);
						switch stream {
							case [l = parseTypeAnonymous(false)]: TExtend(tl,l);
							case [fl = parseClassFields(true, p1)]: TExtend(tl, fl.fields);
							case _: unexpected();
						}
					case [l = parseClassFields(true, p1)]: TAnonymous(l.fields);
					case _: unexpected();
				}
			case [{tok:Question}, t = parseComplexTypeInner(allowNamed)]:
				TOptional(t);
			case [{tok:Spread}, t = parseComplexTypeInner(allowNamed)]:
				var hint = switch (t) {
					case TNamed(_, t):
						t;
					case _:
						t;
				}
				TPath({pack: ["haxe"], name: "Rest", params: [TPType(hint)]});
			case [n = dollarIdent()]:
				switch stream {
					case [{tok:DblDot}, t = parseComplexType()] if (allowNamed):
						TNamed(n.name, t);
					case _:
						var t = parseTypePath2([], n);
						TPath(t);
				}
			case [t = parseTypePath()]:
				TPath(t);
		}
	}

	function parseTypePath() {
		return parseTypePath1([]);
	}

	function parseTypePath1(pack:Array<String>) {
		return switch stream {
			case [ident = dollarIdentMacro(pack)]:
				parseTypePath2(pack, ident);
		}
	}


	function parseTypePath2(pack:Array<String>, ident):TypePath {
		if (isLowerIdent(ident.name)) {
			return switch stream {
				case [{tok:Dot}]:
					parseTypePath1(apush(pack, ident.name));
				case [{tok:Semicolon}]:
					throw new ParserError(Custom("Type name should start with an uppercase letter"), ident.pos);
				case _: unexpected();
			}
		} else {
			var sub = switch stream {
				case [{tok:Dot}]:
					switch stream {
						case [{tok:Const(CIdent(name))} && !isLowerIdent(name)]: name;
						case _: unexpected();
					}
				case _:
					null;
			}
			var params = switch stream {
				case [{tok:Binop(OpLt)}, l = psep(Comma, parseTypePathOrConst), {tok:Binop(OpGt)}]: l;
				case _: [];
			}
			return {
				pack: pack,
				name: ident.name,
				params: params,
				sub: sub
			}
		}
	}

	function typeName() {
		return switch stream {
			case [{tok: Const(CIdent(name)), pos:p}]:
				if (isLowerIdent(name)) throw new ParserError(Custom("Type name should start with an uppercase letter"), p);
				else name;
		}
	}

	function parseTypePathOrConst() {
		return switch stream {
			case [{tok:BkOpen, pos: p1}, l = parseArrayDecl(), {tok:BkClose, pos:p2}]: TPExpr({expr: EArrayDecl(l), pos:punion(p1,p2)});
			case [t = parseComplexType()]: TPType(t);
			case [{tok:Unop(op), pos: p1}, {tok:Const(c), pos: p2}]: TPExpr(makeUnop(op, mapConstant(c, p2), p1));
			case [{tok:Binop(OpSub), pos: p1}, {tok:Const(c), pos: p2}]: TPExpr(makeUnop(OpNeg, mapConstant(c, p2), p1));
			case [{tok:Const(c), pos:p}]: TPExpr(mapConstant(c, p));
			case [{tok:Kwd(KwdTrue), pos:p}]: TPExpr({expr:EConst(CIdent("true")), pos:p});
			case [{tok:Kwd(KwdFalse), pos:p}]: TPExpr({expr:EConst(CIdent("false")), pos:p});
			case [e = expr()]: TPExpr(e);
			case _: unexpected();
		}
	}

	function parseComplexTypeNext(t:ComplexType) {
		return switch stream {
			case [{tok:Arrow}, t2 = parseComplexType()]:
				switch(t2) {
					case TFunction(args,r):
						TFunction(aunshift(args,t),r);
					case _:
						TFunction([t],t2);
				}
			case [{tok:Binop(OpAnd), pos:pa}, t2 = parseComplexType()]:
				switch(t2) {
					case TIntersection(tl):
						TIntersection(aunshift(tl, t));
					case _:
						TIntersection([t, t2]);
				}
			case _: t;
		}
	}

	function parseTypeAnonymous(opt:Bool):Array<Field> {
		return switch stream {
			case [id = ident(), t = parseTypeHint()]:
				function next(p2,acc) {
					var t = !opt ? t : switch(t) {
						case TPath({pack:[], name:"Null"}): t;
						case _: TPath({pack:[], name:"Null", sub:null, params:[TPType(t)]});
					}
					return aunshift(acc, {
						name: id.name,
						meta: opt ? [{name:":optional",params:[], pos:id.pos}] : [],
						access: [],
						doc: null,
						kind: FVar(t,null),
						pos: punion(id.pos, p2)
					});
				}
				switch stream {
					case [{tok:BrClose, pos:p2}]: next(p2, []);
					case [{tok:Comma, pos:p2}]:
						switch stream {
							case [{tok:BrClose}]: next(p2, []);
							case [l = parseTypeAnonymous(false)]: next(p2, l);
							case _: unexpected();
						}
					case _: unexpected();
				}
			case [{tok:Question} && !opt]: parseTypeAnonymous(true);
		}
	}

	function parseEnum() {
		doc = null;
		var meta = parseMeta();
		return switch stream {
			case [name = anyEnumIdent(), doc = getDoc(), params = parseConstraintParams()]:
				var args = switch stream {
					case [{tok:POpen}, l = psep(Comma, parseEnumParam), {tok:PClose}]: l;
					case _: [];
				}
				var t = parseTypeOpt();
				var p2 = switch stream {
					case [p = semicolon()]: p;
					case _: unexpected();
				}
				{
					name: name.name,
					doc: doc,
					meta: meta,
					args: args,
					params: params,
					type: t,
					pos: punion(name.pos, p2)
				}
		}
	}

	function parseEnumParam() {
		return switch stream {
			case [{tok:Question}, name = ident(), t = parseTypeHint()]: { name: name.name, opt: true, type: t};
			case [name = ident(), t = parseTypeHint()]: { name: name.name, opt: false, type: t };
		}
	}

	function parseFunctionField(doc, meta, accessList) {
		return switch stream {
			case [{tok:Kwd(KwdFunction), pos:p1}, name = parseFunName(), pl = parseConstraintParams(), {tok:POpen}, al = psep(Comma, parseFunParam), {tok:PClose}, t = parseTypeOpt()]:
				var e = switch stream {
					case [e = toplevelExpr(), _ = semicolon()]:
						{ expr: e, pos: e.pos };
					case [{tok: Semicolon,pos:p}]:
						{ expr: null, pos: p}
					case _: unexpected();
				}
				var f = {
					params: pl,
					args: al,
					ret: t,
					expr: e.expr
				}
				{
					name: name,
					pos: punion(p1, e.pos),
					kind: FFun(f)
				}
		}
	}

	function parseVarFieldAssignment() {
		return switch stream {
			case [{tok:Binop(OpAssign)}, e = toplevelExpr(), p2 = semicolon()]: { expr: e, pos: p2 };
			case [{tok:Semicolon, pos:p2}]: { expr: null, pos: p2 };
			case _: unexpected();
		}
	}

	function parseClassField():Field {
		doc = null;
		return switch stream {
			case [meta = parseMeta(), al = parseCfRights(true,[]), doc = getDoc()]:
				var data = switch stream {
					case [{tok:Kwd(KwdVar), pos:p1}, name = questionableDollarIdent()]:
						switch stream {
							case [{tok:POpen}, i1 = propertyIdent(), {tok:Comma}, i2 = propertyIdent(), {tok:PClose}]:
								var t = parseTypeOpt();
								var e = parseVarFieldAssignment();
								{
									name: name.name,
									pos: punion(p1,e.pos),
									kind: FProp(i1,i2,t,e.expr)
								}
							case [t = parseTypeOpt()]:
								var e = parseVarFieldAssignment();
								{
									name: name.name,
									pos: punion(p1,e.pos),
									kind: FVar(t,e.expr)
								}
						}
					case [{tok:Kwd(KwdFinal), pos:p1}]:
						switch stream {
							case [name = questionableDollarIdent(), t = parseTypeOpt(), e = parseVarFieldAssignment()]:
								al.push(AFinal);
								{
									name: name.name,
									pos: punion(p1,e.pos),
									kind: FVar(t,e.expr)
								}
							case [al2 = parseCfRights(true,al), f = parseFunctionField(doc, meta, apush(al2, AFinal))]:
								al = al2;
								f;
						}
					case [f = parseFunctionField(doc, meta, al)]:
						f;
					case _:
						if (al.length == 0)
							throw noMatch();
						else
							unexpected();
				}
			{
				name: data.name,
				doc: doc,
				meta: meta,
				access: al,
				pos: data.pos,
				kind: data.kind
			}
		}
	}

	function parseCfRights(allowStatic:Bool, l:Array<Access>) {
		return switch stream {
			case [{tok:Kwd(KwdStatic)} && allowStatic, l = parseCfRights(false, apush(l, AStatic))]: l;
			case [{tok:Kwd(KwdMacro)}, l = parseCfRights(allowStatic, apush(l, AMacro))]: l;
			case [{tok:Kwd(KwdPublic)}, l = parseCfRights(allowStatic, apush(l, APublic))]: l;
			case [{tok:Kwd(KwdPrivate)}, l = parseCfRights(allowStatic, apush(l, APrivate))]: l;
			case [{tok:Kwd(KwdOverride)}, l = parseCfRights(false, apush(l, AOverride))]: l;
			case [{tok:Kwd(KwdDynamic)}, l = parseCfRights(allowStatic, apush(l, ADynamic))]: l;
			case [{tok:Kwd(KwdInline)}, l = parseCfRights(allowStatic, apush(l, AInline))]: l;
			case [{tok:Kwd(KwdExtern)}, l = parseCfRights(allowStatic, apush(l, AExtern))]: l;
			case [{tok:Kwd(KwdAbstract)} && allowStatic, l = parseCfRights(false, apush(l, AAbstract))]: l;
			case [{tok:Kwd(KwdOverload)}, l = parseCfRights(allowStatic, apush(l, AOverload))]: l;
			case _: l;
		}
	}

	function parseFunName() {
		return switch stream {
			case [{tok:Const(CIdent(name))}]: name;
			case [{tok:Kwd(KwdNew)}]: "new";
		}
	}

	function parseFunParam() {
		var meta = parseMeta();
		return switch stream {
			case [{tok:Question}, id = dollarIdent(), t = parseTypeOpt(), c = parseFunParamValue()]: { name: id.name, opt: true, type: t, value: c, meta: meta };
			case [id = dollarIdent(), t = parseTypeOpt(), c = parseFunParamValue()]: { name: id.name, opt: false, type: t, value: c, meta: meta };
			case [{tok:Spread}, id = dollarIdent(), t = parseTypeOpt(), c = parseFunParamValue()]:
				if (t == null) {
					t = TPath({pack:["$"], name: "_hx_mono"});
				}
				var t = TPath({pack: ["haxe"], name: "Rest", params: [TPType(t)]});
				{ name: id.name, opt: false, type: t, value: c, meta: meta };
		}
	}

	function parseFunParamValue() {
		return switch stream {
			case [{tok:Binop(OpAssign)}, e = toplevelExpr()]: e;
			case _: null;
		}
	}

	function parseFunParamType() {
		return switch stream {
			case [{tok:Question}, id = ident(), t = parseTypeHint()]: { name: id.name, opt: true, type: t};
			case [ id = ident(), t = parseTypeHint()]: { name: id.name, opt: false, type: t};
		}
	}

	function parseConstraintParams() {
		return switch stream {
			case [{tok:Binop(OpLt)}, l = psep(Comma, parseConstraintParam), {tok:Binop((OpGt))}]: l;
			case _: [];
		}
	}

	function parseConstraintParam() {
		return switch stream {
			case [meta = parseMeta(), name = typeName()]:
				var params = [];
				var ctl = switch stream {
					case [{tok:DblDot}]:
						switch stream {
							case [{tok:POpen}, l = psep(Comma, parseComplexType), {tok:PClose}]: l;
							case [t = parseComplexType()]: [t];
							case _: unexpected();
						}
					case _: [];
				}
				var defaultType = switch stream {
					case [{tok:Binop(OpAssign)}]:
						switch stream {
							case [t = parseComplexType()]: t;
							case _: unexpected();
						}
					case _: null;
				}
				{
					name: name,
					params: params,
					constraints: ctl,
					defaultType: defaultType,
					meta: meta
				}
		}
	}

	function parseClassHerit() {
		return switch stream {
			case [{tok:Kwd(KwdExtends)}, t = parseTypePath()]: HExtends(t);
			case [{tok:Kwd(KwdImplements)}, t = parseTypePath()]: HImplements(t);
		}
	}

	function block1() {
		return switch stream {
			case [{tok:Const(CIdent(name)), pos:p}]: block2(name, Unquoted, CIdent(name), p);
			case [{tok:Const(CString(name, qs)), pos:p}]: block2(quoteIdent(name), Quoted, CString(name, qs), p);
			case [b = block([])]: EBlock(b);
		}
	}

	function block2(name:String, quotes:QuoteStatus, ident:Constant, p:Position) {
		return switch stream {
			case [{tok:DblDot}, e = expr(), l = parseObjDecl()]:
				l.unshift({field:name, expr:e, quotes:quotes});
				EObjectDecl(l);
			case _:
				var e = exprNext({expr:EConst(ident), pos: p});
				var _ = semicolon();
				var b = block([e]);
				EBlock(b);
		}
	}

	function block(acc:Array<Expr>) {
		try {
			var e = parseBlockElt();
			return block(apush(acc,e));
		} catch(e:hxparse.NoMatch<Dynamic>) {
			return acc;
		}
	}

	function parseBlockVar() {
		return switch stream {
			case [{tok:Kwd(KwdVar), pos:p1}, vl = psep(Comma, function() return parseVarDecl(false)), p2 = semicolon()]: { expr: EVars(vl), pos:punion(p1,p2)};
			case [{tok:Kwd(KwdFinal), pos:p1}, vl = psep(Comma, () -> parseVarDecl(true)), p2 = semicolon()]: { expr: EVars(vl), pos:punion(p1,p2)};
		}
	}


	function parseBlockElt() {
		return switch stream {
			case [e = parseBlockVar()]:
				e;
			case [{tok:Kwd(KwdInline), pos:p1}]:
				switch stream {
					case [{tok:Kwd(KwdFunction)}, e = parseFunction(p1, true), _ = semicolon()]:
						e;
					case [e = secureExpr(), _ = semicolon()]:
						makeMeta(":inline", [], e, p1);
					case _:
						unexpected();
				}
			case [{tok:Kwd(KwdStatic), pos:p}]:
				switch stream {
					case [e = parseBlockVar()]:
						switch (e.expr) {
							case EVars(vars):
								for (v in vars) v.isStatic = true;
							case _:
						}
						e;
					case _:
						unexpected();
				}
			case [e = expr(), _ = semicolon()]: e;
		}
	}

	function parseObjDecl() {
		var acc = [];
		while(true) {
			switch stream {
				case [{tok:Comma}]:
					switch stream {
						case [id = ident(), {tok:DblDot}, e = expr()]:
							acc.push({field:id.name, expr: e, quotes: Unquoted});
						case [{tok:Const(CString(name, _))}, {tok:DblDot}, e = expr()]:
							//apush(l,{field:quoteIdent(name), expr: e});
							acc.push({field:quoteIdent(name), expr: e, quotes: Quoted});
						case _:
							break;
					}
				case _:
					break;
			}
		}
		return acc;
	}

	function parseArrayDecl() {
		var acc = [];
		var br = false;
		while(true) {
			switch stream {
				case [e = expr()]:
					acc.push(e);
					switch stream {
						case [{tok: Comma}]:
						case _: br = true;
					}
				case _: br = true;
			}
			if (br) break;
		}
		return acc;
	}

	function parseVarDecl(isFinal:Bool):Var {
		return switch stream {
			case [meta = parseMeta(), id = dollarIdent(), t = parseTypeOpt()]:
				switch stream {
					case [{tok:Binop(OpAssign)}, e = expr()]: { name: id.name, type: t, expr: e, isFinal: isFinal, meta: meta};
					case _: { name: id.name, type:t, expr: null, isFinal: isFinal, meta: meta};
				}
		}
	}

	function inlineFunction() {
		return switch stream {
			case [{tok:Kwd(KwdInline)}, {tok:Kwd(KwdFunction), pos:p1}]: { isInline: true, pos: p1};
			case [{tok:Kwd(KwdFunction), pos: p1}]: { isInline: false, pos: p1};
		}
	}

	function reify(inMacro:Bool) {
		var reificator = new Reificator(inMacro);
		return {
			toExpr: function(e:Expr):Expr{
				return reificator.toExpr(e,e.pos);
			},
			toType: reificator.toCType,
			toTypeDef: reificator.toTypeDef
		};
	}

	function reifyExpr(e:Expr) {
		var toExpr = reify(inMacro).toExpr;
		var e = toExpr(e);
		return { expr: ECheckType(e, TPath( {pack:["haxe","macro"], name:"Expr", sub:null, params: []})), pos: e.pos};
	}

	function parseMacroExpr(p:Position) {
		return switch stream {
			case [{tok:DblDot}, t = parseComplexType()]:
				var toType = reify(inMacro).toType;
				var t = toType(t,p);
				{ expr: ECheckType(t, TPath( {pack:["haxe","macro"], name:"Expr", sub:"ComplexType", params: []})), pos: p};
			case [{tok:Kwd(KwdVar), pos:p1}, vl = psep(Comma, function () return parseVarDecl(false))]:
				reifyExpr({expr:EVars(vl), pos:p1});
			case [{tok:Kwd(KwdFinal), pos:p1}, vl = psep(Comma, function () return parseVarDecl(true))]:
				reifyExpr({expr:EVars(vl), pos:p1});
			case [d = parseClass([],[],false)]:
				var toType = reify(inMacro).toTypeDef;
				{ expr: ECheckType(toType(d), TPath( {pack:["haxe","macro"], name:"Expr", sub:"TypeDefinition", params: []})), pos: p};
			case [e = secureExpr()]:
				reifyExpr(e);
		}
	}

	function parseFunction(p1, inl) {
		return switch stream {
			case [name = parseOptional(dollarIdent), pl = parseConstraintParams(), {tok:POpen}, al = psep(Comma, parseFunParam), {tok:PClose}, t = parseOptional(parseTypeHint)]:
				var make = function(eBody) {
					var f = {
						params:pl,
						ret:t,
						args:al,
						expr:eBody
					};
					var e = {expr:EFunction((name == null) ? FAnonymous : FNamed(name.name), f), pos:punion(p1, eBody.pos)};
					return (inl) ? makeMeta(":inline", [], e, p1) : e;
				}
				make(secureExpr());
		}
	}

	function arrowExpr() {
		return switch stream {
			case [{tok:Arrow}, e = expr()]:
				e;
		}
	}

	function arrowFunction(p1, al, er) {
		return {expr: EFunction(FArrow, {params:[], ret:null, args:al, expr:{expr:EReturn(er), pos:er.pos}}), pos:punion(p1, er.pos)};
	}

	function arrowIdentChecktype (e) {
		return switch (e.expr) {
			case EConst(CIdent(n)):
				{name:n, type:null};
			case ECheckType({expr:EConst(CIdent(n))}, t):
				{name:n, type:t};
			case _:
				unexpected();
		}
	}

	function arrowFirstParam(e:Expr) {
		return switch (e.expr) {
			case EConst(CIdent(n)):
				{name:n, opt:false, meta:[], type:null, value:null};
			case EBinop(op, e1, e2):
				null;
			case EParenthesis({expr:EBinop(OpAssign, e1, e2)}):
				var np = arrowIdentChecktype(e1);
				{name:np.name, opt:true, meta:[], type:np.type, value:e2};
			case EParenthesis(e):
				var np = arrowIdentChecktype(e);
				{name:np.name, opt:false, meta:[], type:np.type, value:null};
			case _:
				unexpected();
		}
	}

	public function expr():Expr {
		return switch stream {
			case [meta = parseMetaEntry()]:
				makeMeta(meta.name, meta.params, secureExpr(), meta.pos);
			case [{tok:BrOpen, pos:p1}, b = block1(), {tok:BrClose, pos:p2}]:
				var e = { expr: b, pos: punion(p1, p2)};
				switch(b) {
					case EObjectDecl(_): exprNext(e);
					case _: e;
				}
			case [{tok:Kwd(KwdMacro), pos:p}]:
				switch stream {
					case [{tok:Dot, pos:pd}, e = parseField({expr:EConst(CIdent("macro")), pos:p}, Normal, pd)]: e;
					case [e = parseMacroExpr(p)]: e;
				}
			case [{tok:Kwd(KwdVar), pos: p1}, v = parseVarDecl(false)]: { expr: EVars([v]), pos: p1};
			case [{tok:Kwd(KwdFinal), pos: p1}, v = parseVarDecl(true)]: { expr: EVars([v]), pos: p1};
			case [{tok:Const(c), pos:p}]: exprNext(mapConstant(c, p));
			case [{tok:Kwd(KwdThis), pos:p}]: exprNext({expr: EConst(CIdent("this")), pos:p});
			case [{tok:Kwd(KwdAbstract), pos:p}]: exprNext({expr: EConst(CIdent("abstract")), pos:p});
			case [{tok:Kwd(KwdTrue), pos:p}]: exprNext({expr: EConst(CIdent("true")), pos:p});
			case [{tok:Kwd(KwdFalse), pos:p}]: exprNext({expr: EConst(CIdent("false")), pos:p});
			case [{tok:Kwd(KwdNull), pos:p}]: exprNext({expr: EConst(CIdent("null")), pos:p});
			case [{tok:Kwd(KwdCast), pos:p1}]:
				switch stream {
					case [{tok:POpen, pos:pp}, e = expr()]:
						switch stream {
							case [{tok:Comma}, t = parseComplexType(), {tok:PClose, pos:p2}]: exprNext({expr:ECast(e,t), pos: punion(p1,p2)});
							case [t = parseTypeHint(), {tok:PClose, pos:p2}]:
								var pu = punion(p1, p2);
								var ep = {expr: EParenthesis({expr: ECheckType(e, t), pos: pu}), pos: pu};
								exprNext({expr: ECast(ep, null), pos: punion(p1, pu)});
							case [{tok:PClose, pos:p2}]:
								var ep = exprNext({expr: EParenthesis(e), pos: punion(pp, p2)});
								exprNext({expr:ECast(ep,null),pos:punion(p1,ep.pos)});
							case _: unexpected();
						}
					case [e = secureExpr()]: exprNext({expr:ECast(e,null), pos:punion(p1, e.pos)});
				}
			case [{tok:Kwd(KwdThrow), pos:p}, e = expr()]: { expr: EThrow(e), pos: p};
			case [{tok:Kwd(KwdNew), pos:p1}, t = parseTypePath(), {tok:POpen, pos:_}]:
				switch stream {
					case [al = psep(Comma, expr), {tok:PClose, pos:p2}]: exprNext({expr:ENew(t,al), pos:punion(p1, p2)});
					case _: unexpected();
				}
			case [{tok:POpen, pos: p1}]:
				switch stream {
					case [{tok:PClose, pos:p2}, er = arrowExpr()]:
						arrowFunction(p1, [], er);
					case [{tok:Question}, al = psep(Comma, parseFunParam), {tok:PClose}, er = arrowExpr()]:
						if (al.length > 0) {
							al[1].opt = true;
						}
						arrowFunction (p1, al, er);
					case [e = expr()]:
						switch stream {
							case [{tok:PClose, pos:p2}]:
								exprNext({expr: EParenthesis(e), pos:punion(p1, p2)});
							case [{tok:Comma}, al = psep(Comma, parseFunParam), {tok:PClose}, er = arrowExpr()]:
								arrowFunction (p1, aunshift(al, arrowFirstParam(e)), er);
							case [t = parseTypeHint()]: {
								switch stream {
									case [{tok:PClose, pos:p2}]:
										exprNext({expr: EParenthesis({expr:ECheckType(e, t), pos:punion(p1, p2)}), pos:punion(p1, p2)});
									case [{tok:Comma, pos:p2}, al = psep(Comma, parseFunParam), {tok:PClose}, er = arrowExpr()]:
										var np = arrowIdentChecktype(e);
										arrowFunction (p1, aunshift(al, {name:np.name, opt:false, meta:[], type:t, value:null}), er);
									case [{tok:Binop(OpAssign), pos:p2}, ea1 = expr()]:
										var withArgs = function(al, er) {
											return switch (e.expr) {
												case EConst(CIdent(n)):
													arrowFunction (p1, aunshift(al, {name:n, opt:true, meta:[], type:t, value:ea1}), er);
												case _:
													unexpected();
											}
										}
										switch stream {
											case [{tok:PClose}, er = arrowExpr()]:
												withArgs([], er);
											case [{tok:Comma}, al = psep(Comma, parseFunParam), {tok:PClose}, er = arrowExpr()]:
												withArgs(al, er);
											case _:
												unexpected();
										}
								}
							}
							case _: unexpected();
						}
				}
			case [{tok:BkOpen, pos:p1}, l = parseArrayDecl(), {tok:BkClose, pos:p2}]: exprNext({expr: EArrayDecl(l), pos:punion(p1,p2)});
			case [{tok:Kwd(KwdFunction), pos:p1}, e = parseFunction(p1, false)]: e;
			case [{tok:Unop(op), pos:p1}, e = expr()]: makeUnop(op, e, p1);
			case [{tok:Spread, pos:p1}, e = expr()]: makeUnop(OpSpread, e, p1);
			case [{tok:Binop(OpSub), pos:p1}, e = expr()]:
				makeUnop(OpNeg, e, p1);
			case [{tok:Kwd(KwdFor), pos:p}, {tok:POpen}, it = expr(), {tok:PClose}]:
				var e = secureExpr();
				{ expr: EFor(it,e), pos:punion(p, e.pos)};
			case [{tok:Kwd(KwdIf), pos:p}, {tok:POpen}, cond = expr(), {tok:PClose}, e1 = expr()]:
				var e2 = switch stream {
					case [{tok:Kwd(KwdElse)}, e2 = expr()]: e2;
					case _:
						switch [peek(0),peek(1)] {
							case [{tok:Semicolon}, {tok:Kwd(KwdElse)}]:
								junk();
								junk();
								secureExpr();
							case _: null;
						}
				}
				{ expr: EIf(cond,e1,e2), pos:punion(p, e2 == null ? e1.pos : e2.pos)};
			case [{tok:Kwd(KwdReturn), pos:p}, e = parseOptional(expr)]: { expr: EReturn(e), pos: e == null ? p : punion(p,e.pos)};
			case [{tok:Kwd(KwdBreak), pos:p}]: { expr: EBreak, pos: p };
			case [{tok:Kwd(KwdContinue), pos:p}]: { expr: EContinue, pos: p};
			case [{tok:Kwd(KwdWhile), pos:p1}, {tok:POpen}, cond = expr(), {tok:PClose}]:
				var e = secureExpr();
				{ expr: EWhile(cond, e, true), pos: punion(p1, e.pos)};
			case [{tok:Kwd(KwdDo), pos:p1}, e = expr(), {tok:Kwd(KwdWhile)}, {tok:POpen}, cond = expr(), {tok:PClose}]: { expr: EWhile(cond,e,false), pos:punion(p1, e.pos)};
			case [{tok:Kwd(KwdSwitch), pos:p1}, e = expr(), {tok:BrOpen}, cases = parseSwitchCases(), {tok:BrClose, pos:p2}]:
				{ expr: ESwitch(e,cases.cases,cases.def), pos:punion(p1,p2)};
			case [{tok:Kwd(KwdTry), pos:p1}, e = expr(), cl = parseRepeat(parseCatch)]:
				{ expr: ETry(e,cl), pos:p1};
			case [{tok:IntInterval(i), pos:p1}, e2 = expr()]: makeBinop(OpInterval,{expr:EConst(CInt(i, null)), pos:p1}, e2);
			case [{tok:Kwd(KwdUntyped), pos:p1}, e = expr()]: { expr: EUntyped(e), pos:punion(p1,e.pos)};
			case [{tok:Dollar(v), pos:p}]: exprNext({expr:EConst(CIdent("$" + v)), pos:p});
			case [{tok:Kwd(KwdInline), pos:p}, e = secureExpr()]: makeMeta(":inline", [], e, p);
		}
	}

	function toplevelExpr():Expr {
		return expr();
	}

	function exprNext(e1:Expr):Expr {
		return switch stream {
			case [{tok:BrOpen, pos:p1} && isDollarIdent(e1), eparam = expr(), {tok:BrClose,pos:p2}]:
				switch (e1.expr) {
					case EConst(CIdent(n)):
						exprNext({expr: EMeta({name:n, params:[], pos:e1.pos},eparam), pos:punion(p1,p2)});
					case _: throw false;
				}
			case [{tok:Dot, pos:p}, e = parseField(e1, Normal, p)]: e;
			case [{tok:QuestionDot, pos:p}, e = parseField(e1, Safe, p)]: e;
			case [{tok:POpen, pos:_}]:
				switch stream {
					case [params = parseCallParams(), {tok:PClose, pos:p2}]:
						exprNext({expr:ECall(e1,params),pos:punion(e1.pos,p2)});
					case _: unexpected();
				}
			case [{tok:BkOpen}, e2 = expr(), {tok:BkClose, pos:p2}]:
				exprNext({expr:EArray(e1,e2), pos:punion(e1.pos,p2)});
			case [{tok:Arrow}]:
				var er = expr();
				arrowFunction(e1.pos, [arrowFirstParam(e1)], er);
			case [{tok:Binop(OpGt)}]:
				switch stream {
					case [{tok:Binop(OpGt)}]:
						switch stream {
							case [{tok:Binop(OpGt)}]:
								switch stream {
									case [{tok:Binop(OpAssign)}, e2 = expr()]:
										makeBinop(OpAssignOp(OpUShr),e1,e2);
									case [e2 = secureExpr()]: makeBinop(OpUShr,e1,e2);
								}
							case [{tok:Binop(OpAssign)}, e2 = expr()]:
								makeBinop(OpAssignOp(OpShr),e1,e2);
							case [e2 = secureExpr()]:
								makeBinop(OpShr,e1,e2);
						}
					case [{tok:Binop(OpAssign)}]:
						makeBinop(OpGte,e1,secureExpr());
					case [e2 = secureExpr()]:
						makeBinop(OpGt,e1,e2);
				}
			case [{tok:Binop(op)}, e2 = expr()]:
				makeBinop(op,e1,e2);
			case [{tok:Spread}, e2 = expr()]:
				makeBinop(OpInterval,e1,e2);
			case [{tok:Unop(op), pos:p} && isPostfix(e1,op)]:
				exprNext({expr:EUnop(op,true,e1), pos:punion(e1.pos, p)});
			case [{tok:Question}, e2 = expr(), {tok:DblDot}, e3 = expr()]:
				{ expr: ETernary(e1,e2,e3), pos: punion(e1.pos, e3.pos)};
			case [{tok:Kwd(KwdIn)}, e2 = expr()]:
				makeBinop(OpIn,e1,e2);
			case [{tok:Const(CIdent("is"))}, t = parseComplexType()]:
				exprNext({expr: EIs(e1,t), pos: e1.pos});
			case _: e1;
		}
	}


	function parseField(e1, efk, p) {
		return switch stream {
			case [{tok:Kwd(KwdMacro), pos:p2}]:
				exprNext({expr:EField(e1, "macro", efk), pos:punion(e1.pos,p2)});
			case [{tok:Kwd(KwdExtern), pos:p2}]:
				exprNext({expr:EField(e1, "extern", efk), pos:punion(e1.pos, p2)});
			case [{tok:Kwd(KwdFunction), pos:p2}]:
				exprNext({expr:EField(e1, "function", efk), pos:punion(e1.pos, p2)});
			case [{tok:Kwd(KwdNew), pos:p2}]:
				exprNext({expr:EField(e1, "new", efk), pos:punion(e1.pos, p2)});
			case [{tok:Kwd(k), pos:p2}]:
				exprNext({expr:EField(e1, KeywordPrinter.toString(k), efk), pos:punion(e1.pos, p2)});
			case [{tok:Const(CIdent(f)), pos:p2}]:
				exprNext({expr:EField(e1, f, efk), pos:punion(e1.pos, p2)});
			case [{tok:Dollar(v), pos:p2}]:
				exprNext({expr:EField(e1, "$" + v, efk), pos:punion(e1.pos, p2)});
				case _:
					switch(e1) {
						case {expr: EConst(CInt(v, null)), pos:p2} if (p2.max == p.min):
							exprNext({expr:EConst(CFloat(v + ".", null)), pos:punion(p, p2)});
							case _: unexpected();
						}
		}
	}

	function parseGuard() {
		return switch stream {
			case [{tok:Kwd(KwdIf)}, {tok:POpen}, e = expr(), {tok:PClose}]:
				e;
		}
	}

	function parseExprOrVar() {
		return switch stream {
			case [{tok:Kwd(KwdVar),pos:p1}, name = dollarIdent()]: { expr: EVars([{name: name.name, type:null, expr:null}]), pos: p1 };
			case [{tok:Kwd(KwdFinal),pos:p1}, name = dollarIdent()]: { expr: EVars([{name: name.name, type:null, expr:null}]), pos: p1 };
			case [e = expr()]: e;
		}
	}

	function parseSwitchCases() {
		var cases = [];
		var def = null;
		function caseBlock(b:Array<Expr>, p:Position) {
			return if (b.length == 0) {
				null;
			} else switch(b) {
				case [e = macro $b{el}]: e;
				case _: { expr: EBlock(b), pos: p};
			}
		}
		while(true) {
			switch stream {
				case [{tok:Kwd(KwdDefault), pos:p1}, {tok:DblDot}]:
					var b = block([]);
					var e = caseBlock(b, p1);
					if (e == null) {
						e = { expr: null, pos: p1 };
					}
					if (def != null) {
						throw new ParserError(DuplicateDefault, p1);
					}
					def = e;
				case [{tok:Kwd(KwdCase), pos:p1}, el = psep(Comma,parseExprOrVar), eg = parseOptional(parseGuard), {tok:DblDot}]:
					var b = block([]);
					var e = caseBlock(b, p1);
					cases.push({values:el,guard:eg,expr:e});
				case _:
					break;
			}
		}
		return {
			cases: cases,
			def: def
		}
	}

	function parseCatch() {
		return switch stream {
			case [{tok:Kwd(KwdCatch), pos:p}, {tok:POpen}, id = ident(), ]:
				switch stream {
					case [t = parseTypeHint(), {tok:PClose}, e = secureExpr()]:
						{name: id.name, type: t, expr: e}
					case [{tok:PClose}, e = secureExpr()]:
						{name: id.name, type: null, expr: e}
					case _:
						throw new ParserError(MissingType, p);
				}
		}
	}

	function parseCallParams() {
		var ret = [];
		switch stream {
			case [e = expr()]: ret.push(e);
			case _: return [];
		}
		while(true) {
			switch stream {
				case [{tok: Comma}, e = expr()]: ret.push(e);
				case _: break;
			}
		}
		return ret;
	}

	function secureExpr() {
		return expr();
	}
}

private class Reificator{

	var curPos:Null<Expr>;
	var inMacro:Bool;

	public function new(inMacro:Bool){
		this.curPos = null;
		this.inMacro = inMacro;
	}

	function mkEnum(ename:String, name:String, vl:Array<Expr>, p:Position):Expr{
		var constr:Expr = {expr:EConst(CIdent(name)), pos:p};
		switch (vl){
			case []: return constr;
			case _ : return {expr:ECall(constr, vl),pos:p};
		}
	}

	function toConst(c:Constant, p:Position):Expr{
		function cst(n:String, v:String):Expr{
			return mkEnum("Constant", n, [{expr:EConst(CString(v)),pos:p}], p);
		}

		switch(c){
			case CInt(i): return cst("CInt", i);
			case CString(s): return cst("CString", s);
			case CFloat(s): return cst("CFloat", s);
			case CIdent(s): return cst("CIdent", s);
			case CRegexp(r,o): return mkEnum("Constant", "CRegexp", [{expr:EConst(CString(r)),pos:p},{expr:EConst(CString(o)),pos:p}], p);
		}
	}

	function toBinop(o:Binop,p:Position):Expr{
		function op(n:String):Expr{
			return mkEnum("Binop", n, [], p);
		}

		switch(o){
			case OpAdd: return op("OpAdd");
			case OpMult: return op("OpMult");
			case OpDiv: return op("OpDiv");
			case OpSub: return op("OpSub");
			case OpAssign: return op("OpAssign");
			case OpEq: return op("OpEq");
			case OpNotEq: return op("OpNotEq");
			case OpGt: return op("OpGt");
			case OpGte: return op("OpGte");
			case OpLt: return op("OpLt");
			case OpLte: return op("OpLte");
			case OpAnd: return op("OpAnd");
			case OpOr: return op("OpOr");
			case OpXor: return op("OpXor");
			case OpBoolAnd: return op("OpBoolAnd");
			case OpBoolOr: return op("OpBoolOr");
			case OpShl: return op("OpShl");
			case OpShr: return op("OpShr");
			case OpUShr: return op("OpUShr");
			case OpMod: return op("OpMod");
			case OpAssignOp(o): return mkEnum("Binop", "OpAssignOp", [toBinop(o, p)], p);
			case OpInterval: return op("OpInterval");
			case OpArrow: return op("OpArrow");
			case OpIn: return op("OpIn");
			case OpNullCoal: return op("OpNullCoal");
		}
	}

	function toString(s:String, p:Position):Expr{
		var len = s.length;
		if (len>1 && s.charAt(0) == '$') return {expr:EConst(CIdent(s.substr(1))),pos:p};
		else return {expr:EConst(CString(s)),pos:p};
	}

	function toArray<T>(f:T->Position->Expr, a:Array<T>, p:Position):Expr{
		var vals = [];
		for (v in a){
			vals.push(f(v,p));
		}

		var e:Expr = {
		pos:p,
		expr:EArrayDecl(vals)
		};
		return e;
	}

	function toNull(p:Position):Expr{
		return {expr:EConst(CIdent("null")),pos:p};
	}

	function toOpt<T>(f:T -> Position -> Expr, v:Null<T>, p:Position):Expr{
		if (v == null) return toNull(p);
		else return f(v, p);
	}

	function toBool(o:Bool, p:Position):Expr{
		var s:String = o?"true":"false";
		return {expr:EConst(CIdent(s)),pos:p};
	}

	function toObj(fields:Array<{field:String, expr:Expr, quotes:QuoteStatus}>, p:Position):Expr{
		return {expr:EObjectDecl(fields),pos:p};
	}

	function toTParam(t:TypeParam, p:Position):Expr{
		var n:String;
		var v:Expr;
		switch(t){
			case TPType(t):
				n = "TPType";
				v = toCType(t, p);
			case TPExpr(e):
				n = "TPExpr";
				v = toExpr(e, p);
		}

		return mkEnum("TypeParam", n, [v], p);
	}

	function toTPath(t:TypePath, p:Position):Expr{
		var fields:Array<{field:String, expr:Expr, quotes:QuoteStatus}> = [
		{field:"pack",   expr:toArray(toString, t.pack, p), quotes:Unquoted},
		{field:"name",   expr:toString(t.name, p), quotes:Unquoted},
		{field:"params", expr:toArray(toTParam, t.params, p), quotes:Unquoted}
		];
		if(t.sub != null){
			fields.push({field:"sub",expr:toString(t.sub, p), quotes:Unquoted});
		}
		return toObj(fields, p);
	}

	public function toCType(t:ComplexType, p:Position):Expr {
		function ct(n:String, vl:Array<Expr>):Expr {
			return mkEnum("ComplexType", n, vl, p);
		}

		return switch(t){
			case TPath({pack: [], params: [], sub: null, name: n }) if (n.charAt(0) == '$'):
				toString(n, p);
			case TNamed(s,t): ct("TNamed", [toString(s,p), toCType(t,p)]);
			case TPath(t): ct("TPath", [toTPath(t, p)]);
			case TFunction(args, ret): ct("TFunction", [toArray(toCType, args, p), toCType(ret, p)]);
			case TAnonymous(fields): ct("TAnonymous", [toArray(toCField, fields, p)]);
			case TParent(t): ct("TParent", [toCType(t, p)]);
			case TExtend(tl, fields): ct("TExtend", [toArray(toTPath, tl, p), toArray(toCField, fields, p)]);
			case TOptional(t): ct("TOptional", [toCType(t, p)]);
			case TIntersection(types): ct("TIntersection", [toArray(toCType, types, p)]);
		}
	}

	function toFun(f:Function, p:Position):Expr{
		function farg(vv:FunctionArg,p:Position):Expr{
			var n = vv.name;
			var o = vv.opt;
			var t = vv.type;
			var e = vv.value;
			var fields:Array<{field:String, expr:Expr, quotes:QuoteStatus}> = [
			{field:"name", expr:toString(n, p), quotes:Unquoted},
			{field:"opt",  expr:toBool(o, p), quotes:Unquoted},
			{field:"type", expr:toOpt(toCType, t, p), quotes:Unquoted}
			];
			if (e != null){
				fields.push({field:"value", expr:toExpr(e, p), quotes:Unquoted});
			}
			return toObj(fields, p);
		}

		function fparam(t:TypeParamDecl,p:Position):Expr{
			var fields:Array<{field:String, expr:Expr, quotes:QuoteStatus}> = [
			{field:"name",        expr:toString(t.name, p), quotes:Unquoted},
			{field:"constraints", expr:toArray(toCType, t.constraints, p), quotes:Unquoted},
			{field:"params",      expr:toArray(fparam, t.params, p), quotes:Unquoted}
			];
			return toObj(fields, p);
		}

		var fields:Array<{field:String, expr:Expr, quotes:QuoteStatus}> = [
		{field:"args",   expr:toArray(farg, f.args, p), quotes:Unquoted},
		{field:"ret",    expr:toOpt(toCType, f.ret, p), quotes:Unquoted},
		{field:"expr",   expr:toOpt(toExpr, f.expr, p), quotes:Unquoted},
		{field:"params", expr:toArray(fparam, f.params, p), quotes:Unquoted}
		];

		return toObj(fields, p);
	}

	function toAccess(a:Access, p:Position):Expr {
		var n = switch(a){
			case APublic:   "APublic";
			case APrivate:  "APrivate";
			case AStatic:   "AStatic";
			case AOverride: "AOverride";
			case ADynamic:  "ADynamic";
			case AInline:   "AInline";
			case AMacro:    "AMacro";
			case AFinal:    "AFinal";
			case AExtern:   "AExtern";
			case AAbstract: "AAbstract";
			case AOverload: "AOverload";
			#if (haxe >= version("5.0.0-alpha"))
			case AEnum:     "AEnum";
			#end
		}
		return mkEnum("Access", n, [], p);
	}

	function toDisplaykind(dk:DisplayKind, p:Position):Expr {
		var n = switch(dk) {
			case DKCall       : "DKCall";
			case DKDot        : "DKDot";
			case DKStructure  : "DKStructure";
			case DKMarked     : "DKMarked";
			case DKPattern(_) : "DKPattern";
		}
		return mkEnum("DisplayKind", n, [], p);
	}

	function toCField(f:Field, p:Position):Expr {
		var p2:Position = f.pos;

		function toFType(k:FieldType):Expr {
			var n:String;
			var vl:Array<Expr>;
			switch(k){
				case FVar(ct, e): n = "FVar"; vl = [toOpt(toCType, ct, p), toOpt(toExpr, e, p)];
				case FFun(f): n = "FFun"; vl = [toFun(f, p)];
				case FProp(get, set, t, e): n = "FProp"; vl = [toString(get, p), toString(set, p), toOpt(toCType, t, p), toOpt(toExpr, e, p)];
			}
			return mkEnum("FieldType", n, vl, p);
		}

		var fields:Array<{field:String, expr:Expr, quotes:QuoteStatus}> = [];
		fields.push({field:"name", expr:toString(f.name, p), quotes:Unquoted});
		if (f.doc != null) fields.push({field:"doc", expr:toString(f.doc, p), quotes:Unquoted});
		if (f.access != null) fields.push({field:"access", expr:toArray(toAccess, f.access, p), quotes:Unquoted});
		fields.push({field:"kind", expr:toFType(f.kind), quotes:Unquoted});
		fields.push({field:"pos",  expr:toPos(f.pos), quotes:Unquoted});
		if (f.meta != null) fields.push({field:"meta", expr:toMeta(f.meta, p), quotes:Unquoted});

		return toObj(fields, p);
	}

	function toMeta(m:Metadata, p:Position):Expr {
		return toArray(function(me:MetadataEntry, _:Position):Expr {
			var fields:Array<{field:String, expr:Expr, quotes:QuoteStatus}> = [
			{field:"name",   expr:toString(me.name, me.pos), quotes:Unquoted},
			{field:"params", expr:toExprArray(me.params, me.pos), quotes:Unquoted},
			{field:"pos",    expr:toPos(me.pos), quotes:Unquoted}
			];
			return toObj(fields, me.pos);
		}, m, p);
	}

	function toPos(p:Position):Expr {
		if (curPos != null) return curPos;

		var file:Expr = {expr:EConst(CString(p.file)),         pos:p};
		var pmin:Expr = {expr:EConst(CInt(Std.string(p.min))), pos:p};
		var pmax:Expr = {expr:EConst(CInt(Std.string(p.max))), pos:p};
		if (inMacro)
			return {expr:EUntyped({expr:ECall({expr:EConst(CIdent("$mk_pos")), pos:p}, [file, pmin, pmax]), pos:p}), pos:p};
		else
			return toObj([{field:"file", expr:file, quotes:Unquoted}, {field:"min", expr:pmin, quotes:Unquoted}, {field:"max", expr:pmax, quotes:Unquoted}], p);
	}
	function toEncPos(p:Position):Expr {
		if (curPos != null) return curPos;
		if (inMacro)
			return toPos(p);
		else
			return {expr:ECall({expr:EField({expr:EField({expr:EField({expr:EConst(CIdent("haxe")), pos:p}, "macro"), pos:p}, "Context"), pos:p}, "makePosition"), pos:p}, [toPos(p)]), pos:p};
	}
	function toExprArray(a:Array<Expr>, p:Position):Expr {
		if (a.length > 0){
			switch(a[0].expr){
			case EMeta(md,e1):
				if (md.name == "$a" && md.params.length == 0){
					switch(e1.expr){
					case EArrayDecl(el): return toExprArray(el, p);
					default: return e1;
					}
				}
			default:
			}
		}
		return toArray(toExpr, a, p);
	}

	public function toExpr(e:Expr, _:Position):Expr {
		var p = e.pos;
		function expr(n:String, vl:Array<Expr>):Expr {
			var e = mkEnum("ExprDef", n, vl, p);
			return toObj([{field:"expr", expr:e, quotes:Unquoted}, {field:"pos", expr:toPos(p), quotes:Unquoted}], p);
		}
		function loop(e:Expr):Expr {
			return toExpr(e, e.pos);
		}
		return switch(e.expr){
			case EConst(CIdent(n)) if (n.charAt(0) == '$' && n.length > 1):
				toString(n, p);
			case EConst(c):
				expr("EConst", [toConst(c, p)]);
			case EArray(e1, e2):
				expr("EArray", [loop(e1), loop(e2)]);
			case EBinop(op, e1, e2):
				expr("EBinop", [toBinop(op, p), loop(e1), loop(e2)]);
			case EField(e, s):
				expr("EField", [loop(e), toString(s, p)]);
			case EParenthesis(e):
				expr("EParenthesis", [loop(e)]);
			case EObjectDecl(fl):
				expr("EObjectDecl", [toArray(function(f:{field:String, expr:Expr}, p2:Position):Expr {return toObj([{field:"field", expr:toString(f.field, p), quotes:Unquoted}, {field:"expr", expr:loop(f.expr), quotes:Unquoted}], p2);}, fl, p)]);
			case EArrayDecl(el):
				expr("EArrayDecl", [toExprArray(el, p)]);
			case ECall(e, el):
				expr("ECall", [loop(e), toExprArray(el, p)]);
			case ENew(t, el):
				expr("ENew", [toTPath(t, p), toExprArray(el, p)]);
			case EUnop(op, isPostfix, e):
				var ops:String;
				switch(op){
					case OpIncrement: ops = "OpIncrement";
					case OpDecrement: ops = "OpDecrement";
					case OpNot: ops = "OpNot";
					case OpNeg: ops = "OpNeg";
					case OpNegBits: ops = "OpNegBits";
					case OpSpread: ops = "OpSpread";
				}
				var op2 = mkEnum("Unop", ops, [], p);
				expr("EUnop", [op2, toBool(isPostfix, p), loop(e)]);
			case EVars(vl):
				expr("EVars", [toArray(function(vv:Var, p:Position):Expr {
					var name = vv.name;
					var type = vv.type;
					var expr = vv.expr;
					var isFinal = vv.isFinal;
					var isStatic = vv.isStatic;
					var meta = vv.meta;
					var fields:Array<{field:String, expr:Expr, quotes:QuoteStatus}> = [
						{field:"name", expr:toString(name, p), quotes:Unquoted},
						{field:"type", expr:toOpt(toCType, type, p), quotes:Unquoted},
						{field:"expr", expr:toOpt(toExpr, expr, p), quotes:Unquoted},
						{field:"isFinal", expr:toBool(isFinal, p), quotes:Unquoted},
						{field:"isStatic", expr:toBool(isStatic, p), quotes:Unquoted},
						{field:"meta", expr:toMeta(meta, p), quotes:Unquoted}
					];
					return toObj(fields, p);
				}, vl, p)]);
			case EFunction(kind, f):
				var kind = switch (kind) {
					case FAnonymous: mkEnum("FunctionKind", "FAnonymous", [], p);
					case FNamed(name, inlined): mkEnum("FunctionKind", "FNamed", [toString(name, p), toOpt(toBool, inlined, p)], p);
					case FArrow: mkEnum("FunctionKind", "FArrow", [], p);
				}
				expr("EFunction", [kind, toFun(f, p)]);
			case EBlock(el):
				expr("EBlock", [toExprArray(el, p)]);
			case EFor(e1, e2):
				expr("EFor", [loop(e1), loop(e2)]);
			case EIf(e1, e2, eelse):
				expr("EIf", [loop(e1), loop(e2), toOpt(toExpr, eelse, p)]);
			case EWhile(e1, e2, normalWhile):
				expr("EWhile", [loop(e1), loop(e2), toBool(normalWhile, p)]);
			case ESwitch(e1, cases, def):
				function scase(swc:Case, p:Position):Expr {
					var el = swc.values;
					var eg = swc.guard;
					var e = swc.expr;
					return toObj([{field:"values", expr:toExprArray(el, p), quotes:Unquoted}, {field:"guard", expr:toOpt(toExpr, eg, p), quotes:Unquoted}, {field:"expr", expr:toOpt(toExpr, e, p), quotes:Unquoted}], p);
				}
				expr("ESwitch", [loop(e1), toArray(scase, cases, p), toOpt(
					function(def2:Null<Expr>, p:Position):Expr {
						return toOpt(function(def3:Expr, p:Position):Expr {
							return toExpr(def3, p);
						}, def2, p);
					}, def, p)]);
			case ETry(e1, catches):
				function scatch(c:Catch, p:Position):Expr {
					var n = c.name;
					var t = c.type;
					var e = c.expr;
					return toObj([{field:"name", expr:toString(n, p), quotes:Unquoted}, {field:"type", expr:toCType(t, p), quotes:Unquoted}, {field:"expr", expr:loop(e), quotes:Unquoted}], p);
				}
				expr("ETry", [loop(e1), toArray(scatch, catches, p)]);
			case EReturn(eo):
				expr("EReturn", [toOpt(toExpr, eo, p)]);
			case EBreak:
				expr("EBreak", []);
			case EContinue:
				expr("EContinue", []);
			case EUntyped(e):
				expr("EUntyped", [loop(e)]);
			case EThrow(e):
				expr("EThrow", [loop(e)]);
			case ECast(e, ct):
				expr("ECast", [loop(e), toOpt(toCType, ct, p)]);
			case EIs(e, ct):
				expr("EIs", [loop(e), toCType(ct, p)]);
			case EDisplay(e, dk):
				expr("EDisplay", [loop(e), toDisplaykind(dk, p)]);
			case ETernary(e1, e2, e3):
				expr("ETernary", [loop(e1), loop(e2), loop(e3)]);
			case ECheckType(e1, ct):
				expr("ECheckType", [loop(e1), toCType(ct, p)]);
			case EMeta(md, e1):
				switch(md.name){
				case "$" | "$e":
					e1;
				case "$a":
					switch(e1.expr){
					case EArrayDecl(el): expr("EArrayDecl", [toExprArray(el, p)]);
					default: expr("EArrayDecl", [e1]);
					}
				case "$b":
					expr("EBlock", [e1]);
				case "$v":
					{expr:ECall(
						{expr:EField(
							{expr:EField(
								{expr:EField(
									{expr:EConst(CIdent("haxe")),
									pos:p},
									"macro"
								),
								pos:p},
								"Context"
							),
							pos:p},
							"makeExpr"
						),
						pos:p},
						[e, toPos(e.pos)]
					),
					pos:p};
				case "$i":
					expr("EConst", [mkEnum("Constant", "CIdent", [e1], e1.pos)]);
				case "$p":
					{expr:ECall(
						{expr:EField(
							{expr:EField(
								{expr:EField(
									{expr:EConst(CIdent("haxe")),
									pos:p},
									"macro"
								),
								pos:p},
								"ExprTools"
							),
							pos:p},
							"toFieldExpr"
						),
						pos:p},
						[e]
					),
					pos:p};
				case ":pos" if (md.params.length == 1):
					var old = curPos;
					curPos = md.params[0];
					var e = loop(e1);
					curPos = old;
					e;
				default: expr("EMeta", [
						toObj([
							{field:"name",expr:toString(md.name, p), quotes:Unquoted},
							{field:"params",expr:toExprArray(md.params, p), quotes:Unquoted},
							{field:"pos",expr:toPos(p), quotes:Unquoted}
						], p),
						loop(e1)
					]);
				}
		}
	}

	function toTParamDecl(t:TypeParamDecl, p:Position):Expr{
		var params = [];
		for (tp in t.params){
			params.push(toTParamDecl(tp,p));
		}

		var constraints = [];
		for (c in t.constraints){
			constraints.push(toCType(c,p));
		}

		return toObj([
			{field:"name", expr:toString(t.name,p), quotes:Unquoted},
			{field:"params", expr:{expr:EArrayDecl(params),pos:p}, quotes:Unquoted},
			{field:"meta", expr:toMeta(t.meta,p), quotes:Unquoted},
			{field:"constraints", expr:{expr:EArrayDecl(constraints),pos:p}, quotes:Unquoted}
		],p);
	}

	public function toTypeDef(td:TypeDecl):Expr{
		var p = td.pos;

		switch(td.decl){
		case EClass(d):
			var ext = null;
			var impl = [];
			var interf = false;
			var isFinal = false;
			var isAbstract = false;

			for (f in d.flags){
				switch(f){
					case HExtern | HPrivate:
					case HFinal: isFinal = true;
					case HInterface: interf = true;
					case HExtends(t): ext = toTPath(t, td.pos);
					case HImplements(i): impl.push(toTPath(i, td.pos));
					case HAbstract: isAbstract = true;
				}
			}

			var params = [];
			for (par in d.params){
				params.push(toTParamDecl(par,p));
			}

			var isExtern = false;
			for (f in d.flags){
				switch(f){
				case HExtern: isExtern = true; break;
				default:
				}
			}

			var kindParams = [];

			if (ext == null){
				kindParams.push({expr:EConst(CIdent("null")),pos:p});
			}
			else {
				kindParams.push(ext);
			}

			kindParams.push({expr:EArrayDecl(impl),pos:p});
			kindParams.push(toBool(interf, p));
			kindParams.push(toBool(isFinal, p));
			kindParams.push(toBool(isAbstract, p));

			var fields = [];
			for (d in d.data){
				fields.push(toCField(d,p));
			}

			return toObj([
				{field:"pack", expr:{expr:EArrayDecl([]),pos:p}, quotes:Unquoted},
				{field:"name", expr:toString(d.name, p), quotes:Unquoted},
				{field:"pos", expr:(toPos(p)), quotes:Unquoted},
				{field:"meta", expr:toMeta(d.meta, p), quotes:Unquoted},
				{field:"params", expr:{expr:EArrayDecl(params),pos:p}, quotes:Unquoted},
				{field:"isExtern", expr:toBool(isExtern, p), quotes:Unquoted},
				{field:"kind", expr:mkEnum("TypeDefKind", "TDClass", kindParams, p), quotes:Unquoted},
				{field:"fields", expr:{expr:EArrayDecl(fields), pos:p}, quotes:Unquoted}
			], td.pos);
		default: throw "Invalid type for reification";
		}
	}
}

function mapConstant(c:haxeparser.Data.Constant, p:Position):haxe.macro.Expr {
	var constant:haxe.macro.Expr.Constant = switch (c) {
		case CInt(v, s):
			CInt(v, s);
		case CFloat(f, s):
			CFloat(f, s);
		case CIdent(s):
			CIdent(s);
		case CString(s, kind):
			CString(s, kind);
		case CRegexp(r, opt):
			CRegexp(r, opt);
		case CMarkup(s):
			CString(s);
	};

	var expr = {expr: EConst(constant), pos:p};
	return switch (c) {
		case CInt(_)| CFloat(_)| CIdent(_)| CString(_)| CRegexp(_):
			expr;
		case CMarkup(_):
			{expr: EMeta({name: ":markup", pos:p}, expr), pos:p};
	};
}
