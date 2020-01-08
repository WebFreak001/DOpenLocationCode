# DOpenLocationCode

Port of the [open-location-code](https://github.com/google/open-location-code) library (also known as [plus codes](https://plus.codes)) for identifying geo areas.

This ports commit (a6eb95b)[https://github.com/google/open-location-code/commit/a6eb95b4d2f934e94dae4f092260caf7f3db7967]

## Example

normal usage:

```d
import openlocationcode;

// get geo coordinates from plus code
OpenLocationCode plusCode = OpenLocationCode.fromString("8FVC2222+22GCCCC");
writeln(plusCode.decode());
// -> 

// get plus code from geo coordinates
OpenLocationCode generatedPlusCode = OpenLocationCode.encode(lat, lon);
writeln(generatedPlusCode.code);
```

---

`nothrow @nogc` usage:

```d
import openlocationcode;

// get geo coordinates from plus code
string input = "8FVC2222+22GCCCC";
if (!input.isValidCode)
	return error;
OpenLocationCode plusCode = OpenLocationCode.fromTrustedString(input);

if (!plusCode.isFull)
	return error;
OpenLocationCodeArea area = plusCode.decodeTrustedFull();
printf("area around %d, %d", area.centerLatitude, area.centerLongitude);

// get plus code from geo coordinates
ubyte[maxOLCLength] buffer;
scope ubyte[] generatedPlusCodeString = OpenLocationCode.encode(buffer, lat, lon);
printf("%s", cast(char[])generatedPlusCodeString);

```