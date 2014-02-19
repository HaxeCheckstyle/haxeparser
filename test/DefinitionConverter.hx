import haxeparser.Data;
import haxe.macro.Expr;

using Lambda;

class DefinitionConverter {
	static public function convertTypeDef(pack:Array<String>, t:TypeDef):TypeDefinition {
		var td = switch(t) {
			case EClass(d): convertClass(d);
			case EEnum(d): convertEnum(d);
			case EAbstract(d): convertAbstract(d);
			//case ETypedef(d): convertTypedef(d);
			case _: throw 'Cannot convert $t';
		}
		td.pack = pack;
		return td;
	}
	
	static function getGeneralDefinition<A, B>(d:Definition<A, B>) {
		return {
			pack: [],
			name: d.name,
			meta: d.meta,
			params: d.params,
			pos: null,
			isExtern: false,
			kind: TDStructure,
			fields: []
		}
	}
	
	static function convertClass(c:Definition<ClassFlag, Array<Field>>) {
		var def = getGeneralDefinition(c);
		var isInterface = false;
		var superClass = null;
		var implementsList = [];
		for (flag in c.flags) {
			switch(flag) {
				case HInterface: isInterface = true;
				case HExtern: def.isExtern = true;
				case HExtends(t): superClass = t;
				case HImplements(t): implementsList.push(t);
				case HPrivate: // TODO: ignored?
			}
		}
		def.fields = c.data;
		def.kind = TDClass(superClass, implementsList, isInterface);
		return def;
	}
	
	static function convertAbstract(a:Definition<AbstractFlag, Array<Field>>) {
		var def = getGeneralDefinition(a);
		var to = [];
		var from = [];
		var thisT = null;
		for (flag in a.flags) {
			switch(flag) {
				case AFromType(t): from.push(t);
				case AToType(t): to.push(t);
				case AIsType(t): thisT = t;
				case APrivAbstract:
			}
		}
		def.fields = a.data;
		def.kind = TDAbstract(thisT, from, to);
		return def;
	}

	static function enumConstructorToClassField(ctor:EnumConstructor) {
		var kind = if(ctor.args.length == 0) {
			FVar(ctor.type, null);
		} else {
			FFun({
				params: ctor.params,
				expr: null,
				ret: ctor.type,
				args: ctor.args.map(function(arg) return {
					name: arg.name,
					opt: arg.opt,
					type: arg.type,
					value: null
				})
			});
		}
		return {
			name: ctor.name,
			doc: ctor.doc,
			access: [],
			pos: ctor.pos,
			meta: ctor.meta,
			kind: kind
		}
	}
	
	static function convertEnum(en:Definition<EnumFlag, Array<EnumConstructor>>) {
		var def = getGeneralDefinition(en);
		def.kind = TDEnum;
		def.fields = en.data.map(enumConstructorToClassField);
		return def;
	}
}