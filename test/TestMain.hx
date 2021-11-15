import utest.ITest;
import haxe.Exception;

import utest.Runner;

import utest.ui.text.DiagnosticsReport;

class TestMain {
    public static function main () : Void
		{
			var tests : Array<ITest> = [
				new Test (),
				#if haxe_std_path new TestStd() #end
			];

			try
			{
				final runner : Runner = new Runner ();

				#if instrument
                runner.onComplete.add(_ -> {
                        instrument.coverage.Coverage.endCoverage();
                });
                #end
				new DiagnosticsReport (runner);

				for (test in tests)
				{
					runner.addCase (test);
				}
				runner.run ();
			}
			catch (e : Exception)
			{
				Sys.println ('${e.details ()}');
			}
		}

}
