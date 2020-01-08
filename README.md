# DOpenLocationCode

Port of the [open-location-code](https://github.com/google/open-location-code) library (also known as [plus codes](https://plus.codes)) for identifying geo areas.

This ports commit [a6eb95b](https://github.com/google/open-location-code/commit/a6eb95b4d2f934e94dae4f092260caf7f3db7967).

## Example

normal usage:

```d
import openlocationcode;

double lat = 54;
double lon = 4;

// get geo coordinates from plus code string
OpenLocationCode plusCode = OpenLocationCode.fromString("8FVC2222+22GCCCC");
writeln(plusCode.decode());
// -> OpenLocationCodeArea(47.0001, 8.00006, 47.0001, 8.00006, 15)

// get plus code string from geo coordinates
OpenLocationCode generatedPlusCode = OpenLocationCode.encode(lat, lon);
writeln(generatedPlusCode.code);
// -> 9F662222+22
```

---

`nothrow @nogc` usage:

```d
import openlocationcode;

double lat = 54;
double lon = 4;

// get geo coordinates from plus code string
string input = "8FVC2222+22GCCCC";
// make sure to only use uppercase characters in your input!
if (!input.isValidUppercaseCode)
	return error;
OpenLocationCode plusCode = OpenLocationCode.fromTrustedString(input);

if (!plusCode.isFull)
	return error;
OpenLocationCodeArea area = plusCode.decodeTrustedFull();
printf("area around %f, %f\n", area.centerLatitude, area.centerLongitude);
// -> area around 47.000062, 8.000063

// get plus code string from geo coordinates
ubyte[maxOLCLength] buffer;
scope ubyte[] generatedPlusCodeString = OpenLocationCode.encode(buffer, lat, lon);
printf("%.*s\n", generatedPlusCodeString.length, &generatedPlusCodeString[0]);
// -> 9F662222+22
```