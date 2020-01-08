import openlocationcode;

import core.time : MonoTime;
import std.random;
import std.stdio;

void main()
{
	enum loops = 1000000;

	struct TestData
	{
		double lat, lon;
		int length;
		string code;

		static TestData generate()
		{
			TestData ret;
			ret.lat = uniform01!double * 180 - 90;
			ret.lon = uniform01!double * 360 - 180;
			ret.length = uniform(4, 15);
			if (ret.length < 10 && ret.length % 2 == 1)
				ret.length++;
			ret.code = OpenLocationCode.encode(ret.lat, ret.lon, ret.length).code;
			return ret;
		}
	}

	// get geo coordinates from plus code string
	OpenLocationCode plusCode = OpenLocationCode.fromString("8FVC2222+22GCCCC");
	writeln(plusCode.decode());

	// get plus code string from geo coordinates
	OpenLocationCode generatedPlusCode = OpenLocationCode.encode(54, 4);
	writeln(generatedPlusCode.code);

	testNogc();

	TestData[] list;
	list.length = loops;
	foreach (i; 0 .. loops)
		list[i] = TestData.generate();

	writeln("Running encode benchmark");
	ubyte[maxOLCLength] buffer = void;
	// benchmark encode
	auto start = MonoTime.currTime();
	foreach (data; list)
		cast(void) OpenLocationCode.encode(buffer[], data.lat, data.lon, data.length);
	auto end = MonoTime.currTime();
	auto dur = end - start;
	writefln("Encode %s loops in %s;\n\t%s per call", loops, dur, dur / loops);

	writeln("Running decode benchmark");
	// benchmark decode
	start = MonoTime.currTime();
	foreach (data; list)
		cast(void) OpenLocationCode.fromTrustedString(data.code).decodeTrustedFull();
	end = MonoTime.currTime();
	dur = end - start;
	writefln("Decode %s loops in %s;\n\t%s per call", loops, dur, dur / loops);
}

void testNogc() @nogc
{
	import core.stdc.stdio;

	// get geo coordinates from plus code string
	string input = "8FVC2222+22GCCCC";
	if (!input.isValidUppercaseCode)
		return;
	OpenLocationCode plusCode = OpenLocationCode.fromTrustedString(input);

	if (!plusCode.isFull)
		return;
	OpenLocationCodeArea area = plusCode.decodeTrustedFull();
	printf("area around %f, %f\n", area.centerLatitude, area.centerLongitude);

	// get plus code string from geo coordinates
	ubyte[maxOLCLength] buffer;
	scope ubyte[] generatedPlusCodeString = OpenLocationCode.encode(buffer, 54, 4);
	printf("%.*s\n", generatedPlusCodeString.length, &generatedPlusCodeString[0]);
}
