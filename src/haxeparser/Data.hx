package haxeparser;

import haxe.macro.Expr;

enum Keyword {
	KwdFunction;
	KwdClass;
	KwdVar;
	KwdIf;
	KwdElse;
	KwdWhile;
	KwdDo;
	KwdFor;
	KwdBreak;
	KwdContinue;
	KwdReturn;
	KwdExtends;
	KwdImplements;
	KwdImport;
	KwdSwitch;
	KwdCase;
	KwdDefault;
	KwdStatic;
	KwdPublic;
	KwdPrivate;
	KwdTry;
	KwdCatch;
	KwdNew;
	KwdThis;
	KwdThrow;
	KwdExtern;
	KwdEnum;
	KwdIn;
	KwdInterface;
	KwdUntyped;
	KwdCast;
	KwdOverride;
	KwdTypedef;
	KwdDynamic;
	KwdPackage;
	KwdInline;
	KwdUsing;
	KwdNull;
	KwdTrue;
	KwdFalse;
	KwdAbstract;
	KwdMacro;
	KwdFinal;
	KwdOperator;
	KwdOverload;
}

class KeywordPrinter {
	static public function toString(kwd:Keyword) {
		return switch(kwd) {
			case KwdFunction: "function";
			case KwdClass: "class";
			case KwdVar: "var";
			case KwdIf: "if";
			case KwdElse: "else";
			case KwdWhile: "while";
			case KwdDo: "do";
			case KwdFor: "for";
			case KwdBreak: "break";
			case KwdContinue: "continue";
			case KwdReturn: "return";
			case KwdExtends: "extends";
			case KwdImplements: "implements";
			case KwdImport: "import";
			case KwdSwitch: "switch";
			case KwdCase: "case";
			case KwdDefault: "default";
			case KwdStatic: "static";
			case KwdPublic: "public";
			case KwdPrivate: "private";
			case KwdTry: "try";
			case KwdCatch: "catch";
			case KwdNew: "new";
			case KwdThis: "this";
			case KwdThrow: "throw";
			case KwdExtern: "extern";
			case KwdEnum: "enum";
			case KwdIn: "in";
			case KwdInterface: "interface";
			case KwdUntyped: "untyped";
			case KwdCast: "cast";
			case KwdOverride: "override";
			case KwdTypedef: "typedef";
			case KwdDynamic: "dynamic";
			case KwdPackage: "package";
			case KwdInline: "inline";
			case KwdUsing: "using";
			case KwdNull: "null";
			case KwdTrue: "true";
			case KwdFalse: "false";
			case KwdAbstract: "abstract";
			case KwdMacro: "macro";
			case KwdFinal: "final";
			case KwdOperator: "operator";
			case KwdOverload: "overload";
		}
	}
}

enum TokenDef {
	Eof;
	Const(c:Constant);
	Kwd(k:Keyword);
	Comment(s:String);
	CommentLine(s:String);
	Binop(op:haxe.macro.Expr.Binop);
	Unop(op:haxe.macro.Expr.Unop);
	Semicolon;
	Comma;
	BrOpen;
	BrClose;
	BkOpen;
	BkClose;
	POpen;
	PClose;
	Dot;
	DblDot;
	QuestionDot;
	Arrow;
	IntInterval(s:String);
	Sharp(s:String);
	Question;
	At;
	Dollar(s:String);
	Spread;
}

enum Constant {
	CInt(v:String, ?s:String);
	CFloat(f:String, ?s:String);
	CString(s:String, ?kind:StringLiteralKind);
	CIdent(s:String);
	CRegexp(r:String, opt:String);
	CMarkup(s:String);
}

class TokenDefPrinter {
	static public function toString(def:TokenDef) {
		return switch(def) {
			case Eof: "<eof>";
			case Const(const): constToString(const);
			case Kwd(k): k.getName().substr(3).toLowerCase();
			case Comment(s): '/*$s*/';
			case CommentLine(s): '//$s';
			case Binop(op): new haxe.macro.Printer("").printBinop(op);
			case Unop(op): new haxe.macro.Printer("").printUnop(op);
			case Semicolon: ";";
			case Comma: ",";
			case BkOpen: "[";
			case BkClose: "]";
			case BrOpen: "{";
			case BrClose: "}";
			case POpen: "(";
			case PClose: ")";
			case Dot: ".";
			case DblDot: ":";
			case QuestionDot: "?.";
			case Arrow: "->";
			case IntInterval(s): '$s...';
			case Sharp(s): '#$s';
			case Question: "?";
			case At: "@";
			case Dollar(s): '$$$s';
			case Spread: "...";
		}
	}


	static public function constToString(const:Constant) {
		return switch (const) {
			case CInt(v, null):
				v;
			case CInt(v, s):
				'$v$s';
			case CFloat(f, null):
				f;
			case CFloat(f, s):
				'$f$s';
			case CString(s, kind):
				switch (kind) {
					case null | DoubleQuotes:
						'"$s"';
					case SingleQuotes:
						'\'$s\'';
				}
			case CIdent(s):
				s;
			case CRegexp(r, opt):
				'~/$r/$opt';
			case CMarkup(s):
				s;
		}
	}

	@:deprecated("Use toString() instead")
	static public inline function print(def:TokenDef) {
		return toString(def);
	}
}

class Token {
	public var tok: TokenDef;
	public var pos: Position;
	#if keep_whitespace
	public var space = "";
	#end
	public function new(tok, pos) {
		this.tok = tok;
		this.pos = pos;
	}

	public function toString() {
		return TokenDefPrinter.toString(tok);
	}
}

typedef EnumConstructor = {
	name : String,
	doc: String,
	meta: Metadata,
	args: Array<{ name: String, opt: Bool, type: ComplexType}>,
	pos: Position,
	params: Array<TypeParamDecl>,
	type: Null<ComplexType>
}

typedef Definition<A,B> = {
	name : String,
	doc: String,
	params: Array<TypeParamDecl>,
	meta: Metadata,
	flags: Array<A>,
	data: B
}

enum TypeDef {
	EClass(d:Definition<ClassFlag, Array<Field>>);
	EEnum(d:Definition<EnumFlag, Array<EnumConstructor>>);
	ETypedef(d:Definition<EnumFlag, ComplexType>);
	EAbstract(a:Definition<haxeparser.AbstractFlag, Array<Field>>);
	EStatic(s:Definition<StaticFlag, FieldType>);
	EImport(sl:Array<{pack:String, pos:Position}>, mode:ImportMode);
	EUsing(path:TypePath);
}

typedef TypeDecl = {
	decl : TypeDef,
	pos : Position
}

enum ClassFlag {
	HInterface;
	HExtern;
	HPrivate;
	HExtends(t:TypePath);
	HImplements(t:TypePath);
	HFinal;
	HAbstract;
}

enum AbstractFlag {
	AbPrivate;
	AbFrom(ct:ComplexType);
	AbTo(ct:ComplexType);
	AbOver(ct:ComplexType);
	AbExtern;
	AbEnum;
}

enum EnumFlag {
	EPrivate;
	EExtern;
}

enum StaticFlag {
	SDynamic;
	SFinal;
	SInline;
	SMacro;
	SPrivate;
	SOverload;
}

enum ImportMode {
	INormal;
	IAsName(s:String);
	IAll;
}
