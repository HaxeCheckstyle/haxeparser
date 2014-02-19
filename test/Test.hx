import haxeparser.Data;
import haxeparser.HaxeLexer;
import haxeparser.HaxeParser;

class Test extends haxe.unit.TestCase {
	
	static function main() {
		var r = new haxe.unit.TestRunner();
		r.add(new Test());
		r.run();
	}
}