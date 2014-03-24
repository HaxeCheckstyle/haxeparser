import haxe.io.Path.*;
import sys.FileSystem.*;

using StringTools;

class TestStd extends haxe.unit.TestCase {

	function test() {
		var basePath = haxe.macro.Compiler.getDefine("haxe_std_path");
		var hasError = false;
		
		function parse(path:String) {
			var content = sys.io.File.getContent(path);
			var input = byte.ByteData.ofString(content);
			try {
				hxparse.Utils.catchErrors(input, function() {
					var parser = new haxeparser.HaxeParser(input, path);
					try {
						parser.parse();
					} catch(e:haxeparser.HaxeParser.ParserError) {
						var pMsg = new hxparse.Position(e.pos.file, e.pos.min, e.pos.max).format(input);
						throw('$pMsg: ${e.msg}\n');
					}
				});
			} catch(e:Dynamic) {
				hasError = true;
				print('$e\n');
			}
		}
		function read(dir:String) {
			for (file in readDirectory(dir)) {
				var fullPath = join([dir, file]);
				switch (file) {
					case "." | "..":
					case _ if (isDirectory(fullPath)):
						read(fullPath);
					case _.endsWith(".hx") => true:
						parse(fullPath);
					case _:
				}
			}
		}
		read(normalize(basePath));
		assertFalse(hasError);
	}
}