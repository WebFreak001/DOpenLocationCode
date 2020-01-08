// Copyright 2014 Google Inc. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * Convert locations to and from convenient short codes.
 *
 * Open Location Codes are short, ~10 character codes that can be used instead of street
 * addresses. The codes can be generated and decoded offline, and use a reduced character set that
 * minimises the chance of codes including words.
 *
 * Ported from the Java interface.
 *
 * @author Jiri Semecky
 * @author Doug Rinckes
 * @author Jan Jurzitza
 */
module openlocationcode;
@safe:

import std.algorithm;
import std.array;
import std.format;
import std.math;
import std.string;

/// Provides a normal precision code, approximately 14x14 meters.
enum codePrecisionNormal = 10;

/// The character set used to encode the values.
static immutable ubyte[] codeAlphabet = "23456789CFGHJMPQRVWX".representation;

/// A separator used to break the code into two parts to aid memorability.
enum separator = '+';

/// The character used to pad codes.
enum paddingCharacter = '0';

/// The number of characters to place before the separator.
private enum separatorPosition = 8;

/// The max number of digits to process in a plus code.
enum maxDigitCount = 15;

/// The max number of characters in an open location code. This is the max number of digits plus a separator character.
enum maxOLCLength = maxDigitCount + 1;

/// Maximum code length using just lat/lng pair encoding.
private enum pairCodeLength = 10;

/// Number of digits in the grid coding section.
private enum gridCodeLength = maxDigitCount - pairCodeLength;

/// The base to use to convert numbers to/from.
private enum encodingBase = cast(int) codeAlphabet.length;

/// The maximum value for latitude in degrees.
private enum long latitudeMax = 90;

/// The maximum value for longitude in degrees.
private enum long longitudeMax = 180;

/// Number of columns in the grid refinement method.
private enum gridColumns = 4;

/// Number of rows in the grid refinement method.
private enum gridRows = 5;

/// Value to multiple latitude degrees to convert it to an integer with the maximum encoding
/// precision. I.e. encodingBase**3 * gridRows**gridCodeLength
private enum long latIntegerMultiplier = 8000 * 3125;

/// Value to multiple longitude degrees to convert it to an integer with the maximum encoding
/// precision. I.e. encodingBase**3 * gridColumns**gridCodeLength
private enum long lngIntegerMultiplier = 8000 * 1024;

/// Value of the most significant latitude digit after it has been converted to an integer.
private enum long latMspValue = latIntegerMultiplier * encodingBase * encodingBase;

/// Value of the most significant longitude digit after it has been converted to an integer.
private enum long lngMspValue = lngIntegerMultiplier * encodingBase * encodingBase;

/**
 * Coordinates of a decoded Open Location Code.
 *
 * The coordinates include the latitude and longitude of the lower left and upper right corners
 * and the center of the bounding box for the area the code represents.
 */
struct OpenLocationCodeArea
{
@nogc nothrow pure:
	private double _southLatitude = 0;
	private double _westLongitude = 0;
	private double _northLatitude = 0;
	private double _eastLongitude = 0;
	private int _length;

	/// Creats this coordinate area from min/max longitude and latitude and a code length.
	this(double southLatitude, double westLongitude, double northLatitude,
			double eastLongitude, int length)
	{
		_southLatitude = southLatitude;
		_westLongitude = westLongitude;
		_northLatitude = northLatitude;
		_eastLongitude = eastLongitude;
		_length = length;
	}

	double southLatitude() const @property
	{
		return _southLatitude;
	}

	double westLongitude() const @property
	{
		return _westLongitude;
	}

	double latitudeHeight() const @property
	{
		return _northLatitude - _southLatitude;
	}

	double longitudeWidth() const @property
	{
		return _eastLongitude - _westLongitude;
	}

	double centerLatitude() const @property
	{
		return (_southLatitude + _northLatitude) * 0.5;
	}

	double centerLongitude() const @property
	{
		return (_westLongitude + _eastLongitude) * 0.5;
	}

	double northLatitude() const @property
	{
		return _northLatitude;
	}

	double eastLongitude() const @property
	{
		return _eastLongitude;
	}

	int length() const @property
	{
		return _length;
	}

	/// Returns: `true` if this area contains the given coordinate.
	bool contains(double latitude, double longitude) const
	{
		return southLatitude <= latitude && latitude < northLatitude
			&& westLongitude <= longitude && longitude < eastLongitude;
	}
}

/// Thrown in methods validating a user input string code.
class OLCFormatException : Exception
{
	///
	this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable nextInChain = null) pure nothrow @nogc @safe
	{
		super(msg, file, line, nextInChain);
	}
}

/// Represents an open location code.
struct OpenLocationCode
{
pure:
	/// The current code for the object
	private immutable(ubyte)[] _code; // note: ubyte[] because it's only ascii

	string code() const @property nothrow @nogc @trusted
	{
		return cast(string) _code;
	}

	/**
	 * Creates Open Location Code object for the provided code.
	 *
	 * Params:
	 * 	code = A valid OLC code. Can be a full code or a shortened code.
	 * Throws: $(D OLCFormatException) when the passed code is not valid.
	 */
	static OpenLocationCode fromString(string code)
	{
		code = code.toUpper;
		if (!isValidCode(code))
		{
			throw new OLCFormatException(
					format!"The provided code '%s' is not a valid Open Location Code."(code));
		}
		return fromTrustedString(code);
	}

	/**
	 * Creates Open Location Code object for the provided code performing no validation. This should call isValidCode before and abort if not valid.
	 *
	 * Params:
	 * 	code = A valid OLC code. Can be a full code or a shortened code.
	 */
	static OpenLocationCode fromTrustedString(string code) nothrow @nogc
	{
		// trim to max length, valid check might allow more characters
		return OpenLocationCode(code.representation[0 .. min(maxOLCLength, $)]);
	}

	/**
	 * Creates Open Location Code.
	 *
	 * Params:
	 * 	buffer = The character buffer to write to. (ASCII only = ubyte[]) Should be at least maxOLCLength in size to avoid crashes.
	 * 	latitude = The latitude in decimal degrees.
	 * 	longitude = The longitude in decimal degrees.
	 * 	codeLength = The desired number of digits in the code. Defaults to the default precision. Must be a valid code length, otherwise contract violation = assertion error.
	 */
	static OpenLocationCode encode(double latitude, double longitude,
			int codeLength = codePrecisionNormal) nothrow
	{
		ubyte[maxOLCLength] buffer;
		auto ret = encode(buffer[], latitude, longitude, codeLength);
		return OpenLocationCode(ret.idup);
	}

	/// ditto
	static ubyte[] encode(scope return ubyte[] buffer, double latitude,
			double longitude, int codeLength = codePrecisionNormal) nothrow @nogc
	in(codeLength.isValidCodeLength, "Illegal code length")
	{
		// Limit the maximum number of digits in the code.
		codeLength = min(codeLength, maxDigitCount);
		// Ensure that latitude and longitude are valid.
		latitude = clipLatitude(latitude);
		longitude = normalizeLongitude(longitude);

		// Latitude 90 needs to be adjusted to be just less, so the returned code can also be decoded.
		if (latitude == latitudeMax)
		{
			latitude = latitude - 0.9 * computeLatitudePrecision(codeLength);
		}

		// Store the code - we build it in reverse and reorder it afterwards.
		auto len = 0;

		// Compute the code.
		// This approach converts each value to an integer after multiplying it by
		// the final precision. This allows us to use only integer operations, so
		// avoiding any accumulation of floating point representation errors.

		// Multiply values by their precision and convert to positive. Rounding
		// avoids/minimises errors due to floating point precision.
		long latVal = cast(long)(round((latitude + latitudeMax) * latIntegerMultiplier * 1e6) / 1e6);
		long lngVal = cast(long)(round((longitude + longitudeMax) * lngIntegerMultiplier * 1e6) / 1e6);

		// Compute the grid part of the code if necessary.
		if (codeLength > pairCodeLength)
		{
			foreach (i; 0 .. gridCodeLength)
			{
				const latDigit = latVal % gridRows;
				const lngDigit = lngVal % gridColumns;
				int ndx = cast(int)(latDigit * gridColumns + lngDigit);
				buffer[len++] = codeAlphabet[ndx];
				latVal /= gridRows;
				lngVal /= gridColumns;
			}
		}
		else
		{
			latVal = cast(long)(latVal / pow(gridRows, gridCodeLength));
			lngVal = cast(long)(lngVal / pow(gridColumns, gridCodeLength));
		}
		// Compute the pair section of the code.
		for (int i = 0; i < pairCodeLength / 2; i++)
		{
			buffer[len++] = codeAlphabet[cast(int)(lngVal % encodingBase)];
			buffer[len++] = codeAlphabet[cast(int)(latVal % encodingBase)];
			latVal /= encodingBase;
			lngVal /= encodingBase;
			// If we are at the separator position, add the separator.
			if (i == 0)
			{
				buffer[len++] = separator;
			}
		}
		// Reverse the code.
		auto codeBuilder = buffer[0 .. len];
		codeBuilder.reverse();

		// If we need to pad the code, replace some of the digits.
		if (codeLength < separatorPosition)
		{
			if (codeBuilder.length < separatorPosition)
			{
				codeBuilder = buffer[0 .. separatorPosition];
				codeBuilder[len .. $] = paddingCharacter;
			}
			else
			{
				codeBuilder[codeLength .. separatorPosition] = paddingCharacter;
			}
		}
		return codeBuilder[0 .. max(separatorPosition + 1, codeLength + 1)];
	}

	/**
	 * Decodes this object into a $(D OpenLocationCodeArea) object encapsulating latitude/longitude bounding box. This method doesn't allocate when not throwing an exception.
	 *
	 * Returns: A OpenLocationCodeArea object.
	 *
	 * Throws: $(D OLCFormatException) if this is not a valid full code.
	 */
	OpenLocationCodeArea decode() const
	{
		if (!isFull)
		{
			throw new OLCFormatException(
					format!"Method decode() could only be called on valid full codes, code was '%s'."(code));
		}
		return decodeTrustedFull();
	}

	/// ditto
	static OpenLocationCodeArea decode(string code)
	{
		return OpenLocationCode.fromString(code).decode();
	}

	/**
	 * Decodes this object into a $(D OpenLocationCodeArea) object encapsulating latitude/longitude bounding box. This method doesn't allocate when not throwing an exception.
	 *
	 * Returns: A OpenLocationCodeArea object.
	 */
	OpenLocationCodeArea decodeTrustedFull() const nothrow @nogc
	in(isFull)
	{
		// Strip padding and separator characters out of the code.
		ubyte[maxOLCLength] buffer;
		int buflen;
		foreach (c; _code)
			if (c != separator && c != paddingCharacter)
				buffer[buflen++] = c;
		auto code = buffer[0 .. buflen];

		// Initialise the values. We work them out as integers and convert them to doubles at the end.
		long latVal = -latitudeMax * latIntegerMultiplier;
		long lngVal = -longitudeMax * lngIntegerMultiplier;
		// Define the place value for the digits. We'll divide this down as we work through the code.
		long latPlaceVal = latMspValue;
		long lngPlaceVal = lngMspValue;
		for (int i = 0; i < min(code.length, pairCodeLength); i += 2)
		{
			latPlaceVal /= encodingBase;
			lngPlaceVal /= encodingBase;
			latVal += codeAlphabet.countUntil(code[i]) * latPlaceVal;
			lngVal += codeAlphabet.countUntil(code[i + 1]) * lngPlaceVal;
		}
		for (int i = pairCodeLength; i < min(code.length, maxDigitCount); i++)
		{
			latPlaceVal /= gridRows;
			lngPlaceVal /= gridColumns;
			const digit = codeAlphabet.countUntil(code[i]);
			const row = digit / gridColumns;
			const col = digit % gridColumns;
			latVal += row * latPlaceVal;
			lngVal += col * lngPlaceVal;
		}
		const latitudeLo = cast(double) latVal / latIntegerMultiplier;
		const longitudeLo = cast(double) lngVal / lngIntegerMultiplier;
		const latitudeHi = cast(double)(latVal + latPlaceVal) / latIntegerMultiplier;
		const longitudeHi = cast(double)(lngVal + lngPlaceVal) / lngIntegerMultiplier;
		return OpenLocationCodeArea(latitudeLo, longitudeLo, latitudeHi,
				longitudeHi, min(code.length, maxDigitCount));
	}

	/**
   * Returns whether this $(D OpenLocationCode) is a full Open Location Code.
   *
   * Returns: `true` if it is a full code.
   */
	bool isFull() const @property nothrow @nogc
	{
		return _code.length > separatorPosition && _code[separatorPosition] == separator;
	}

	/**
   * Returns whether this $(D OpenLocationCode) is a short Open Location Code.
   *
   * Returns: True if it is short.
   */
	bool isShort() const @property nothrow @nogc
	{
		const index = _code.countUntil(separator);
		return index >= 0 && index < separatorPosition;
	}

	/**
   * Returns whether this $(D OpenLocationCode) is a padded Open Location Code, meaning that it
   * contains less than 8 valid digits.
   *
   * @return True if this code is padded.
   */
	bool isPadded() const @property nothrow @nogc
	{
		return _code.canFind(paddingCharacter);
	}

	/**
	 * Returns short $(D OpenLocationCode) from the full Open Location Code created by removing
	 * four or six digits, depending on the provided reference point. It removes as many digits as
	 * possible.
	 *
	 * Params:
	 * 	referenceLatitude = Reference point latitude degrees.
	 * 	referenceLongitude = Reference point longitude degrees.
	 * Returns: A short code if possible or OpenLocationCode.init if it is too far away.
	 * Throws: $(D OLCFormatException) if this is not called on a valid full code or called on a padded code.
	 */
	OpenLocationCode shorten(double referenceLatitude, double referenceLongitude) const
	{
		if (!isFull())
			throw new OLCFormatException("shorten() method could only be called on a full code.");
		if (isPadded())
			throw new OLCFormatException("shorten() method can not be called on a padded code.");
		return shortenTrustedFullNoPad(referenceLatitude, referenceLongitude);
	}

	/**
	 * Returns short $(D OpenLocationCode) from the full Open Location Code created by removing
	 * four or six digits, depending on the provided reference point. It removes as many digits as
	 * possible. Assumes this is a full code which isn't padded.
	 *
	 * Params:
	 * 	referenceLatitude = Reference point latitude degrees.
	 * 	referenceLongitude = Reference point longitude degrees.
	 * Returns: A short code if possible or OpenLocationCode.init if it is too far away.
	 */
	OpenLocationCode shortenTrustedFullNoPad(double referenceLatitude, double referenceLongitude) const nothrow @nogc
	{
		const codeArea = decodeTrustedFull();
		const range = max(abs(referenceLatitude - codeArea.centerLatitude),
				abs(referenceLongitude - codeArea.centerLongitude));
		// We are going to check to see if we can remove three pairs, two pairs or just one pair of
		// digits from the code.
		for (int i = 4; i >= 1; i--)
		{
			// Check if we're close enough to shorten. The range must be less than 1/2
			// the precision to shorten at all, and we want to allow some safety, so
			// use 0.3 instead of 0.5 as a multiplier.
			if (range < (computeLatitudePrecision(i * 2) * 0.3))
			{
				// We're done.
				return OpenLocationCode(_code[i * 2 .. $]);
			}
		}
		return OpenLocationCode.init;
	}

	/**
	 * Recover the nearest match (if the code was a short code) representing a full Open
	 * Location Code from this (short) Open Location Code, given the reference location.
	 *
	 * Params:
	 * 	referenceLatitude = Reference point latitude degrees.
	 * 	referenceLongitude = Reference point longitude degrees.
	 * @return The nearest matching full code.
	 */
	OpenLocationCode recover(double referenceLatitude, double referenceLongitude) const
	{
		if (isFull)
		{
			// Note: each code is either full xor short, no other option.
			return this;
		}
		referenceLatitude = clipLatitude(referenceLatitude);
		referenceLongitude = normalizeLongitude(referenceLongitude);

		const digitsToRecover = separatorPosition - _code.countUntil(separator);
		// The precision (height and width) of the missing prefix in degrees.
		const prefixPrecision = pow(cast(double) encodingBase, 2.0 - (digitsToRecover / 2));

		// Use the reference location to generate the prefix.
		auto recoveredPrefix = OpenLocationCode.encode(referenceLatitude,
				referenceLongitude)._code[0 .. digitsToRecover];
		// Combine the prefix with the short code and decode it.
		const recovered = OpenLocationCode(recoveredPrefix ~ _code);
		const recoveredCodeArea = recovered.decode();
		// Work out whether the new code area is too far from the reference location. If it is, we
		// move it. It can only be out by a single precision step.
		double recoveredLatitude = recoveredCodeArea.centerLatitude;
		double recoveredLongitude = recoveredCodeArea.centerLongitude;

		// Move the recovered latitude by one precision up or down if it is too far from the reference,
		// unless doing so would lead to an invalid latitude.
		const latitudeDiff = recoveredLatitude - referenceLatitude;
		if (latitudeDiff > prefixPrecision / 2 && recoveredLatitude - prefixPrecision > -latitudeMax)
		{
			recoveredLatitude -= prefixPrecision;
		}
		else if (latitudeDiff < -prefixPrecision / 2 && recoveredLatitude + prefixPrecision < latitudeMax)
		{
			recoveredLatitude += prefixPrecision;
		}

		// Move the recovered longitude by one precision up or down if it is too far from the
		// reference.
		const longitudeDiff = recoveredCodeArea.centerLongitude - referenceLongitude;
		if (longitudeDiff > prefixPrecision / 2)
		{
			recoveredLongitude -= prefixPrecision;
		}
		else if (longitudeDiff < -prefixPrecision / 2)
		{
			recoveredLongitude += prefixPrecision;
		}

		return OpenLocationCode.encode(recoveredLatitude, recoveredLongitude,
				(cast(int) recovered.code.length) - 1);
	}

	/**
   * Returns whether the bounding box specified by the Open Location Code contains provided point.
   *
	 * Params:
   * 	latitude = Latitude degrees.
   * 	longitude = Longitude degrees.
   * Returns: $(D true) if the coordinates are contained by the code.
	 * Throws: $(D OLCFormatException) if this is not a valid full code.
   */
	bool contains(double latitude, double longitude) const @safe
	{
		return decode().contains(latitude, longitude);
	}

	/**
   * Returns whether the bounding box specified by the Open Location Code contains provided point.
   *
	 * Params:
   * 	latitude = Latitude degrees.
   * 	longitude = Longitude degrees.
   * Returns: $(D true) if the coordinates are contained by the code.
   */
	bool containsTrustedFull(double latitude, double longitude) const nothrow @nogc @safe
	{
		return decodeTrustedFull().contains(latitude, longitude);
	}

	string toString() const
	{
		return code;
	}
}

/**
 * Returns whether the provided code length 
 */
bool isValidCodeLength(int codeLength) nothrow @nogc pure
{
	const c = min(codeLength, maxDigitCount);
	return c >= 4 && !(c < pairCodeLength && c % 2 == 1);
}

// Exposed static helper methods.

/**
 * Returns whether the provided string is a valid Open Location code.
 *
 * Params:
 * 	codeString = The code to check.
 * Returns: True if it is a valid full or short code.
 */
bool isValidCode(string codeString) pure
{
	return isValidUppercaseCode(codeString.toUpper);
}

/**
 * Same as isValidCode but doesn't convert the code to uppercase before handling, thus failing validation when using lowercase characters.
 */
bool isValidUppercaseCode(string codeString) nothrow @nogc pure
{
	if (codeString.length < 2)
		return false;

	const code = codeString.representation;

	// there must be exactly one separator
	const separatorPosition = code.countUntil(separator);
	if (separatorPosition == -1 || code[separatorPosition + 1 .. $].canFind(separator))
		return false;

	// there must be an even number of at most 8 characters before the separator
	if (separatorPosition % 2 != 0 || separatorPosition > .separatorPosition)
		return false;

	// Check first two characters: only some values from the alphabet are permitted.
	if (separatorPosition == .separatorPosition)
	{
		// First latitude character can only have first 9 values.
		if (codeAlphabet.countUntil(code[0]) > 8)
			return false;

		// First longitude character can only have first 18 values.
		if (codeAlphabet.countUntil(code[1]) > 17)
			return false;
	}

	// Check the characters before the separator.
	bool paddingStarted;
	foreach (i, c; code[0 .. separatorPosition])
	{
		if (!codeAlphabet.canFind(c) && c != paddingCharacter)
			return false; // invalid character

		if (paddingStarted)
		{
			// once padding starts, there must not be anything but padding.
			if (c != paddingCharacter)
				return false;
		}
		else if (c == paddingCharacter)
		{
			paddingStarted = true;

			// short codes cannot have padding
			if (separatorPosition < .separatorPosition)
				return false;

			// padding can start on even character: 2, 4 or 6.
			if (!i.among!(2, 4, 6))
				return false;
		}
	}

	if (code.length > separatorPosition + 1)
	{
		if (paddingStarted)
			return false;

		// only one character after separator is forbidden
		const extra = code[separatorPosition + 1 .. $];
		if (extra.length == 1)
			return false;
		if (extra.any!(a => !codeAlphabet.canFind(a)))
			return false;
	}

	return true;
}

/**
 * Returns if the code is a valid full Open Location Code.
 *
 * Params:
 * 	code = The code to check.
 * Returns: True if it is a valid full code.
 */
bool isFullCode(string code) pure
{
	return code.toUpper.isFullUppercaseCode;
}

/**
 * Does the same as isFullCode, assuming the code is already uppercase, making this @nogc nothrow.
 */
bool isFullUppercaseCode(string code) nothrow @nogc pure
{
	if (!code.isValidUppercaseCode)
		return false;
	return OpenLocationCode.fromTrustedString(code).isFull;
}

/**
 * Returns if the code is a valid short Open Location Code.
 *
 * Params:
 * 	code = The code to check.
 * Returns: True if it is a valid short code.
 */
bool isShortCode(string code) pure
{
	return code.toUpper.isShortUppercaseCode;
}

/**
 * Does the same as isShortCode, assuming the code is already uppercase, making this @nogc nothrow.
 */
bool isShortUppercaseCode(string code) nothrow @nogc pure
{
	if (!code.isValidUppercaseCode)
		return false;
	return OpenLocationCode.fromTrustedString(code).isShort;
}

// Private static methods.

private double clipLatitude(double latitude) nothrow @nogc pure
{
	return min(max(latitude, -latitudeMax), latitudeMax);
}

private double normalizeLongitude(double longitude) nothrow @nogc pure
{
	while (longitude < -longitudeMax)
	{
		longitude = longitude + longitudeMax * 2;
	}
	while (longitude >= longitudeMax)
	{
		longitude = longitude - longitudeMax * 2;
	}
	return longitude;
}

/**
   * Compute the latitude precision value for a given code length. Lengths <= 10 have the same
   * precision for latitude and longitude, but lengths > 10 have different precisions due to the
   * grid method having fewer columns than rows. Copied from the JS implementation.
   */
private double computeLatitudePrecision(int codeLength) nothrow @nogc pure
{
	if (codeLength <= codePrecisionNormal)
		return pow(encodingBase, cast(double)(codeLength / -2 + 2));
	else
		return pow(encodingBase, -3) / pow(gridRows, codeLength - pairCodeLength);
}

// ========== TESTS ==========

version (unittest)
{
	import unit_threaded;

	enum epsilon = 1e-10;

	void epsilonTest(A, B)(A a, B b, in string file = __FILE__, in size_t line = __LINE__)
	{
		shouldApproxEqual(a, b, epsilon);
	}
}

@("test validity")
@system unittest
{
	import std.csv;
	import std.stdio;
	import std.typecons;

	auto file = File("test_data/test_validity.csv", "r");
	foreach (record; file.byLine.filter!(a => !a.startsWith("#")).joiner("\n")
			.csvReader!(Tuple!(string, "code", bool, "isValid", bool, "isShort", bool, "isFull")))
	{
		void test() @safe
		{
			shouldEqual(isValidCode(record.code), record.isValid);
			shouldEqual(isShortCode(record.code), record.isShort);
			shouldEqual(isFullCode(record.code), record.isFull);
		}

		test();
	}
}

@("test shortening")
@system unittest
{
	import std.csv;
	import std.stdio;
	import std.typecons;

	auto file = File("test_data/test_short_codes.csv", "r");
	foreach (record; file.byLine.filter!(a => !a.startsWith("#")).joiner("\n")
			.csvReader!(Tuple!(string, "code", double, "lat", double, "lng", string,
				"shortCode", string, "testType")))
	{
		void testShorten() @safe
		{
			auto olc = OpenLocationCode.fromString(record.code);
			shouldEqual(olc.shorten(record.lat, record.lng).code, record.shortCode);
		}

		void testRecovery() @safe
		{
			auto olc = OpenLocationCode.fromString(record.shortCode);
			shouldEqual(olc.recover(record.lat, record.lng).code, record.code);
		}

		if (record.testType == "B" || record.testType == "S")
			testShorten();

		if (record.testType == "B" || record.testType == "R")
			testRecovery();
	}
}

@("test encoding")
@system unittest
{
	import std.csv;
	import std.stdio;
	import std.typecons;

	auto file = File("test_data/test_encoding.csv", "r");
	foreach (record; file.byLine.filter!(a => !a.startsWith("#")).joiner("\n")
			.csvReader!(Tuple!(double, "lat", double, "lng", int, "length", string, "code")))
	{
		shouldEqual(OpenLocationCode.encode(record.lat, record.lng, record.length).code, record.code);
	}
}

@("test decoding")
@system unittest
{
	import std.csv;
	import std.stdio;
	import std.typecons;

	auto file = File("test_data/test_decoding.csv", "r");
	foreach (record; file.byLine.filter!(a => !a.startsWith("#")).joiner("\n")
			.csvReader!(Tuple!(string, "code", int, "length", double, "latLo",
				double, "lngLo", double, "latHi", double, "lngHi")))
	{
		auto olc = OpenLocationCode.fromString(record.code);
		auto area = olc.decode();
		// test decode
		shouldEqual(record.length, area.length); // Wrong length
		epsilonTest(record.latLo, area.southLatitude); // Wrong low latitude
		epsilonTest(record.latHi, area.northLatitude); // Wrong high latitude
		epsilonTest(record.lngLo, area.westLongitude); // Wrong low longitude
		epsilonTest(record.lngHi, area.eastLongitude); // Wrong high longitude

		// test contains
		shouldBeTrue(olc.contains(area.centerLatitude, area.centerLongitude)); // Containment relation is broken for the decoded middle point of code
		shouldBeTrue(olc.contains(area.southLatitude, area.westLongitude)); // Containment relation is broken for the decoded bottom left corner
		shouldBeFalse(olc.contains(area.northLatitude, area.eastLongitude)); // Containment relation is broken for the decoded top right corner
		shouldBeFalse(olc.contains(area.southLatitude, area.eastLongitude)); // Containment relation is broken for the decoded bottom right corner
		shouldBeFalse(olc.contains(area.northLatitude, area.westLongitude)); // Containment relation is broken for the decoded top left corner
	}
}

@("test clipping")
unittest
{
	shouldEqual(OpenLocationCode.encode(-90, 5), OpenLocationCode.encode(-91, 5));
	shouldEqual(OpenLocationCode.encode(90, 5), OpenLocationCode.encode(91, 5));
	shouldEqual(OpenLocationCode.encode(5, 175), OpenLocationCode.encode(5, -185));
	shouldEqual(OpenLocationCode.encode(5, 175), OpenLocationCode.encode(5, -905));
	shouldEqual(OpenLocationCode.encode(5, -175), OpenLocationCode.encode(5, 905));
}

@("test max code length")
unittest
{
	// Check that we do not return a code longer than is valid.
	string code = OpenLocationCode.encode(51.3701125, -10.202665625, 1_000_000).code;
	shouldEqual(maxDigitCount + 1, code.length); // Encoded code should have a length of maxDigitCount + 1 for the plus symbol
	shouldBeTrue(isValidCode(code));
	// Extend the code with a valid character and make sure it is still valid.
	string tooLongCode = code ~ "W";
	shouldBeTrue(isValidCode(tooLongCode)); // Too long code with all valid characters should be valid.
	// Extend the code with an invalid character and make sure it is invalid.
	tooLongCode = code ~ "U";
	shouldBeFalse(isValidCode(tooLongCode)); // Too long code with invalid character should be invalid.
}

@("test recovery near south pole")
unittest
{
	shouldEqual(OpenLocationCode.fromString("XXXXXX+XX").recover(-81.0, 0.0).code, "2CXXXXXX+XX");
}

@("test recovery near north pole")
unittest
{
	shouldEqual(OpenLocationCode.fromString("2222+22").recover(89.6, 0.0).code, "CFX22222+22");
}

@("test width in degrees")
unittest
{
	epsilonTest(OpenLocationCode.fromString("67000000+").decode().longitudeWidth, 20.);
	epsilonTest(OpenLocationCode.fromString("67890000+").decode().longitudeWidth, 1.);
	epsilonTest(OpenLocationCode.fromString("6789CF00+").decode().longitudeWidth, 0.05);
	epsilonTest(OpenLocationCode.fromString("6789CFGH+").decode().longitudeWidth, 0.0025);
	epsilonTest(OpenLocationCode.fromString("6789CFGH+JM").decode().longitudeWidth, 0.000125);
	epsilonTest(OpenLocationCode.fromString("6789CFGH+JMP").decode().longitudeWidth, 0.00003125);
}

@("test height in degrees")
unittest
{
	epsilonTest(OpenLocationCode.fromString("67000000+").decode().latitudeHeight, 20.);
	epsilonTest(OpenLocationCode.fromString("67890000+").decode().latitudeHeight, 1.);
	epsilonTest(OpenLocationCode.fromString("6789CF00+").decode().latitudeHeight, 0.05);
	epsilonTest(OpenLocationCode.fromString("6789CFGH+").decode().latitudeHeight, 0.0025);
	epsilonTest(OpenLocationCode.fromString("6789CFGH+JM").decode().latitudeHeight, 0.000125);
	epsilonTest(OpenLocationCode.fromString("6789CFGH+JMP").decode().latitudeHeight, 0.000025);
}
