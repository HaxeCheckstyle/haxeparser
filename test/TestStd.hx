import utest.Assert;
import utest.ITest;
import haxe.io.Path.*;
import sys.FileSystem.*;

using StringTools;

class TestStd implements ITest {

	public function new() {}

	function test() {
		var basePath = haxe.macro.Compiler.getDefine("haxe_std_path");
		var hasError = false;
		var numFiles = 0;

		function parse(path:String) {
			var content = sys.io.File.getContent(path);
			var input = byte.ByteData.ofString(content);
			try {
				hxparse.Utils.catchErrors(input, function() {
					var parser = new haxeparser.HaxeParser(input, path);
					parser.define("cross");
					parser.define("scriptable");
					parser.define("unsafe");
					try {
						numFiles++;
						parser.parse();
					} catch(e:haxeparser.HaxeParser.ParserError) {
						switch (e.msg) {
							case SharpError(_):
							case _:
								var pMsg = new hxparse.Position(e.pos.file, e.pos.min, e.pos.max).format(input);
								throw('$pMsg: ${e.msg}\n');
						}
					}
				});
			} catch(e:Dynamic) {
				hasError = true;
				trace('While parsing $path\n');
				trace('$e\n');
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
		if (basePath != null) {
			read(normalize(basePath));
			trace('Parsed $numFiles files');
			Assert.isFalse(hasError);
		}
	}
}
