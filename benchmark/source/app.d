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
