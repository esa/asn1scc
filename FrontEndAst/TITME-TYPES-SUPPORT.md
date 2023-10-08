# ASN.1 TIME TYPES SUPPORT
## The ASN.1 standard
### The TIME type
The ASN.1 TIME type fully supports ISO 8601 (the definitive standard for time and date representations). The main features of ISO 8601 are as follows:

- The identification of date only, of time of day (local time or UTC or both) only, and of date and time.
- The identification of time intervals using either a start and end point, a duration, or a duration with either a start or an end point.
-The concept of specifying a recurring time interval.

The useful time types (DATE, TIME-OF-DAY, DATE-TIME) are defined as subsets of the TIME type.


The latest version of X.680 spec (ASN.1) introduces the concept of property settings. In general, property settings can be applied to any ASN.1 type that has properties. Property settings behave actually like another kind of ASN.1 constraint.
Currently, only the TIME type has properties. 

The TIME properties along with their possible values are summarized below:
```
Basic =  Date | Time | Date-Time | Interval | Rec-Interval

Date = C | Y | YM | YMD | YD | YW | YWD

Year = Basic | Proleptic | Negative | L5 | L6 | L7,  etc

Time = H | HM | HMS | HF1 | HF2 | HF3 | HMF1 | HMF2 | HMF3 | HMSF1 | HMSF2  	  | HMSF3

Local-or-UTC = L | Z | LD

Interval-type = SE | D | SD |DE

SE-point = Date | Time | Date-Time

Midnight = Start | End
```

By applying property settings the protocol designer can customize the TIME type according to his needs. 

#### Basic property values
| Property | Description | Supported by ASN1SCC |
|----------|-------------|----------------------|
| Date | Date only | Yes |
| Time | Time of day only | Yes |
| Date-Time | Date and time of day | Yes |
| Interval | Time interval | No |
| Rec-Interval | Recurring time interval | No |

#### Date property values
| Property | Description | Supported by ASN1SCC |
|----------|-------------|--|
| C | Century only | No |
| Y | Year only | No |
| YM | Year and month | No |
| YMD | Year, month, and day | Yes |
| YD | Year and day of year | No |
| YW | Year and week of year | No |
| YWD | Year, week of year, and day of week | No |

#### Year property values
| Property | Description | Supported by ASN1SCC |
|----------|-------------|--|
| Basic | Basic form (1582 .. 9999) | Yes |
| Proleptic | Proleptic form (0000 .. 1581) | No |
| Negative | Negative form (-9999 .. -0001) | No |
| L5, L6, L7, etc., to infinity (Large) | All abstract values containing a year whose decimal representation requires 5, 6, 7, etc., digits (or a century whose decimal representation requires 3, 4, 5, etc., digits) respectively, whetherpositive or negative | No |


#### Time property values
| Property | Description | Supported by ASN1SCC |
|----------|-------------|--|
| H | Hour only | No |
| HM | Hour and minute | No |
| HMS | Hour, minute, and second | Yes |
| HFn | Hour and fraction of hour, n decimal places where n is positive integer | No |
| HMFn | Hour, minute, and fraction of minute, n decimal place where n is positive integer|  No |
| HMSFn | Hour, minute, second, and fraction of second, n decimal places where n is positive integer |Yes, n<=3 |

#### Local-or-UTC property values
| Property | Description | Supported by ASN1SCC |
|----------|-------------|--|
| L | Local time | Yes |
| Z | UTC time | Yes |
| LD | Local time with difference from UTC | Yes |


Nore detailed descriptions of the TIME properties can be found in the X.680 spec (ASN.1), paragraph 38.

Examples
```
MyDate ::= TIME(SETTINGS "Basic=Date Date=YMD Year=Basic")
```
means that MyDate can keep dates (not times), in the form year, month, date and the year component is in its basic form (i.e. values from 1582 .. 9999).
```
MyDateTime ::= TIME (SETTINGS "Basic=Date-Time Date=YMD Year=Basic  Time = HMS Local-or-UTC = LD")
```
means that MyDateTime can keep both the date and time component. The date component is in the form year, month, date and the year component is in its basic form (i.e. values from 1582 .. 9999). The time component is in the form hour,min, sec in local time with the difference from UTC.


### Useful time types
The standard defines some useful time types. The most important are:
```
DATE ::= TIME (SETTINGS "Basic=Date Date=YMD Year=Basic")

TIME-OF-DAY ::= TIME (SETTINGS "Basic=Time Time=HMS Local-or-UTC=L")

DATE-TIME ::= TIME (SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=L")
```
The above types are supported by ASN1SCC.

## ASN1SCC support for time types
The ASN.1 standard allows plenty variations of property settings forming different TIME types. ASN1SCC will **not** support all of them. ASN1SCC will support the useful time types (DATE, TIME-OF-DAY, DATE-TIME) and some other time types that are useful in the context of embedded systems. The following tables provides a list of supported time types along with some examples.

| ASN.1 time TYPE | Description | Example | AST representation in ASN1SCC | C representation in ASN1SCC | 
|-----------------|-------------|---------| ------------------------------| ---------------------------- |
| DATE | Date only | "2023-12-25" | Asn1Date | Asn1Date |
| TIME-OF-DAY | Time of day only. Refers to local time | "06:30:00" | Asn1LocalTime | Asn1LocalTime |
| TIME (SETTINGS "Basic=Time Time=HMSn Local-or-UTC=L") | Time of day only. Refers to local time. The fraction of seconds has n decimal places | "06:30:00.000" | Asn1LocalTime | Asn1LocalTime |
| TIME (SETTINGS "Basic=Time Time=HMSn Local-or-UTC=Z") | Time of day only. Refers to UTC time. The fraction of seconds has n decimal places | "06:30:00.000Z" | Asn1UtcTime | Asn1UtcTime |
| TIME (SETTINGS "Basic=Time Time=HMSn Local-or-UTC=LD") | Time of day only. Refers to local time with difference from UTC. The fraction of seconds has n decimal places | "06:30:00.000+01:00" | Asn1TimeWithTimeZone | Asn1TimeWithTimeZone |
| DATE-TIME | Date and time of day. Refers to local time | "2023-12-25T06:30:00" | Asn1DateLocalTime | Asn1DateLocalTime |
| TIME (SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMSn Local-or-UTC=L") | Date and time of day. Refers to local time. The fraction of seconds has n decimal places | "2023-12-25T06:30:00.000" | Asn1DateLocalTime | Asn1DateLocalTime |
| TIME (SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMSn Local-or-UTC=Z") | Date and time of day. Refers to UTC time. The fraction of seconds has n decimal places | "2023-12-25T06:30:00.000Z" | Asn1DateUtcTime | Asn1DateUtcTime |
| TIME (SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMSn Local-or-UTC=LD") | Date and time of day. Refers to local time with difference from UTC. The fraction of seconds has n decimal places | "2023-12-25T06:30:00.000+01:00" | Asn1DateTimeWithTimeZone | Asn1DateTimeWithTimeZone |




### Examples of supported time types in ASN.1


```ASN.1
MyDate ::= DATE
myDate1 MyDate ::= "2023-12-25"

MyTimeOfDay ::= TIME-OF-DAY
sixThirtyInTheMorning MyTimeOfDay ::= "06:30:00"

MyDateTime ::= DATE-TIME
myDateTime1 MyDateTime ::= "2023-12-25T06:30:00"

MyLocalDateTimeWithMilliseconds ::= TIME (SETTINGS "Basic=Date-Time Date=YMD Year=Basic  Time=HMS3 Local-or-UTC=L")

newYear2023 MyLocalDateTimeWithMilliseconds ::= "2023-01-01T00:00:00.000"

MyLocalDateTimeWithTimeZoneMilliseconds ::= TIME (SETTINGS "Basic=Date-Time Date=YMD Year=Basic  Time=HMS3 Local-or-UTC=LD")

newYear2023WithTimeZone MyLocalDateTimeWithTimeZoneMilliseconds ::= "2023-01-01T00:00:00.000+01:00"

MyUtcDateTimeWithMilliseconds ::= TIME (SETTINGS "Basic=Date-Time Date=YMD Year=Basic  Time=HMS3 Local-or-UTC=Z")

newYear2023Utc MyUtcDateTimeWithMilliseconds ::= "2023-01-01T00:00:00.000Z"

```

### How time types will be internally represented in the asn1scc AST
The ASN.1 TIME types will be represented in the asn1scc AST using an F# discriminated union as follows:
```fsharp
type TimeTypeClass =
    |Asn1Date                                   //represents the DATE type
    |Asn1LocalTime                      of int  //represents the TIME-OF-DAY type.
    |Asn1UtcTime                        of int  //represents the TIME-OF-DAY type with Local-or-UTC=Z
    |Asn1LocalTimeWithTimeZone          of int  //represents the TIME-OF-DAY type with Local-or-UTC=LD
    |Asn1Date_LocalTime                 of int //represents the DATE-TIME type
    |Asn1Date_UtcTime                   of int //represents the DATE-TIME type with Local-or-UTC=Z
    |Asn1Date_LocalTimeWithTimeZone     of int //represents the DATE-TIME type with Local-or-UTC=LD
(*
In all of the above types, the int parameter represents the number of decimal places of the fraction of seconds.
*)


```

### How time types are represented in C
ASN1SCC represents time types in C as follows:
```C
typedef struct {
	int sign;		//-1 or +1
	int hours;
	int mins;
} Asn1TimeZone;

typedef struct {
	int hours;
	int mins;
	int secs;
	int fraction;
	Asn1TimeZone tz;
} Asn1TimeWithTimeZone;

typedef struct {
	int hours;
	int mins;
	int secs;
	int fraction;
} Asn1UtcTime;

typedef struct {
	int hours;
	int mins;
	int secs;
	int fraction;
} Asn1LocalTime;

typedef struct {
	int years;
	int months;
	int days;
} Asn1Date;

typedef struct {
	Asn1Date	   date;
	Asn1LocalTime  time;
} Asn1DateLocalTime;

typedef struct {
	Asn1Date	 date;
	Asn1UtcTime  time;
} Asn1DateUtcTime;

typedef struct {
	Asn1Date	 date;
	Asn1TimeWithTimeZone  time;
} Asn1DateTimeWithTimeZone;

typedef enum {
	Asn1TC_LocalTimeStamp,
	Asn1TC_UtcTimeStamp,
	Asn1TC_LocalTimeTZStamp
} Asn1TimeZoneClass;

```
Equivalent types will be used in Ada.

### How TIME TYPES are encoded in uPER.
The encoding of TIME types in uPER is described in the X.691 spec (ASN.1 encoding rules). The following table summarizes the encoding of the supported time types in uPER.

| Time type | Encoding |
|-----------|----------|
| DATE | encoded using the DATE-ENCODING |
| TIME-OF-DAY | encoded using the TIME-OF-DAY-ENCODING |
| TIME-OF-DAY-UTC | encoded using the TIME-OF-DAY-UTC-ENCODING |
| TIME-OF-DAY-AND-DIFF | encoded using the TIME-OF-DAY-AND-DIFF-ENCODING |
| TIME-OF-DAY-AND-FRACTION | encoded using the TIME-OF-DAY-AND-FRACTION-ENCODING |
| TIME-OF-DAY-UTC-AND-FRACTION | encoded using the TIME-OF-DAY-UTC-AND-FRACTION-ENCODING |
| TIME-OF-DAY-AND-DIFF-AND-FRACTION | encoded using the TIME-OF-DAY-AND-DIFF-AND-FRACTION-ENCODING |
| DATE-TIME | encoded using the DATE-TIME-ENCODING{DATE-ENCODING, TIME-OF-DAY-ENCODING} |
| DATE-TIME-UTC | encoded using the DATE-TIME-ENCODING{DATE-ENCODING, TIME-OF-DAY-UTC-ENCODING} |
| DATE-TIME-AND-DIFF | encoded using the DATE-TIME-ENCODING{DATE-ENCODING, TIME-OF-DAY-DIFF-ENCODING} |
| DATE-TIME-AND-FRACTION | encoded using the DATE-TIME-ENCODING{DATE-ENCODING, TIME-OF-DAY-AND-FRACTION-ENCODING} |
| DATE-TIME-UTC-AND-FRACTION | encoded using the DATE-TIME-ENCODING{DATE-ENCODING, TIME-OF-DAY-UTC-AND-FRACTION-ENCODING} |
| DATE-TIME-AND-DIFF-AND-FRACTION | encoded using the DATE-TIME-ENCODING{DATE-ENCODING, TIME-OF-DAY-AND-DIFF-AND-FRACTION-ENCODING} |



#### ASN.1 encoding subtypes

```ASN.1

YEAR-ENCODING ::= CHOICE { 	-- 2 bits for choice determinant
	immediate INTEGER (2005..2020), -- 4 bits
	near-future INTEGER (2021..2276), -- 8 bits
	near-past INTEGER (1749..2004), -- 8 bits
	remainder INTEGER (MIN..1748 | 2277..MAX)
}

DATE-ENCODING ::= SEQUENCE {
    year YEAR-ENCODING,
    month INTEGER (1..12),  -- 4 bits
    day INTEGER (1..31)     -- 5 bits -- 
}

TIME-OF-DAY-ENCODING ::= SEQUENCE {
	hours INTEGER (0..24),      -- 5 bits
	minutes INTEGER (0..59),    -- 5 bits
	seconds INTEGER (0..60)     -- 5 bits
}

TIME-OF-DAY-UTC-ENCODING ::= SEQUENCE {
    hours INTEGER (0..24),      -- 5 bits
    minutes INTEGER (0..59),    -- 5 bits
    seconds INTEGER (0..60)     -- 5 bits -- 
}

TIME-DIFFERENCE ::= SEQUENCE {
	sign ENUMERATED { positive, negative },
	hours INTEGER (0..15),
	minutes INTEGER (1..59) OPTIONAL 
}

TIME-OF-DAY-AND-DIFF-ENCODING ::= SEQUENCE {
	local-time SEQUENCE {
		hours INTEGER (0..24),
		minutes INTEGER (0..59),
		seconds INTEGER (0..60) 
	},
	time-difference TIME-DIFFERENCE 
}


TIME-OF-DAY-AND-FRACTION-ENCODING ::= SEQUENCE {
	hours INTEGER (0..24), -- 5 bits
	minutes INTEGER (0..59), -- 5 bits
	seconds INTEGER (0..60), -- 5 bits --
	fraction INTEGER (0..999, ..., 1000..MAX)	-- 11 bits for up to three digits accuracy -- 
}


TIME-OF-DAY-UTC-AND-FRACTION-ENCODING ::= SEQUENCE {
	hours INTEGER (0..24), -- 5 bits
	minutes INTEGER (0..59), -- 5 bits
	seconds INTEGER (0..60), -- 5 bits --
	fraction INTEGER (0..999, ..., 1000..MAX)	-- 11 bits for up to three digits accuracy -- 
}

TIME-OF-DAY-AND-DIFF-AND-FRACTION-ENCODING ::= SEQUENCE {
	local-time SEQUENCE {
		hours INTEGER (0..24),
		minutes INTEGER (0..59),
		seconds INTEGER (0..60),
		fraction INTEGER (0..999, ..., 1000..MAX)
	},
	time-difference TIME-DIFFERENCE 
}


DATE-TIME-ENCODING {Date-Type, Time-Type} ::= SEQUENCE {
	date Date-Type,
	time Time-Type
}
```

### C run time functions

The following table summarizes the basic operation of each function.
| Function | Description |
|----------|-------------|
| _IsValid | Checks if the input is valid |
| _compareTo | Compares two types, returns -1 if the first type is lower than the second, 0 if they are equal and 1 if the first is greater than the second |
| _uper_encode | Encodes the input using uPER |
| _uper_decode | Decodes the input using uPER |

The following C functions will be included in the C run time library.
```C
flag Asn1Date_IsValid(Asn1Date* pThis);
int Asn1Date_compareTo(Asn1Date* pThis, Asn1Date* pOther);
void Asn1Date_uper_encode(BitStream* pBitStrm, const Asn1Date* pThis);
flag Asn1Date_uper_decode(BitStream* pBitStrm, Asn1Date* pThis);

flag Asn1LocalTime_IsValid(int fractionDigits, Asn1LocalTime* pThis);
int Asn1LocalTime_compareTo(int fractionDigits, Asn1LocalTime* pThis, Asn1LocalTime* pOther);
void Asn1LocalTime_uper_encode(BitStream* pBitStrm, int fractionDigits, const Asn1LocalTime* pThis);
flag Asn1LocalTime_uper_decode(BitStream* pBitStrm, int fractionDigits, Asn1LocalTime* pThis);

flag Asn1UtcTime_IsValid(int fractionDigits, Asn1UtcTime* pThis);
int Asn1UtcTime_compareTo(int fractionDigits, Asn1UtcTime* pThis, Asn1UtcTime* pOther);
void Asn1UtcTime_uper_encode(BitStream* pBitStrm, int fractionDigits, const Asn1UtcTime* pThis);
flag Asn1UtcTime_uper_decode(BitStream* pBitStrm, int fractionDigits, Asn1UtcTime* pThis);

flag Asn1TimeWithTimeZone_IsValid(int fractionDigits, Asn1TimeWithTimeZone* pThis);
int Asn1TimeWithTimeZone_compareTo(int fractionDigits, Asn1TimeWithTimeZone* pThis, Asn1TimeWithTimeZone* pOther);
void Asn1TimeWithTimeZone_uper_encode(BitStream* pBitStrm, int fractionDigits, const Asn1TimeWithTimeZone* pThis);
flag Asn1TimeWithTimeZone_uper_decode(BitStream* pBitStrm, int fractionDigits, Asn1TimeWithTimeZone* pThis);

flag Asn1DateLocalTime_IsValid(int fractionDigits, Asn1DateLocalTime* pThis);
int Asn1DateLocalTime_compareTo(int fractionDigits, Asn1DateLocalTime* pThis, Asn1DateLocalTime* pOther);
void Asn1DateLocalTime_uper_encode(BitStream* pBitStrm, int fractionDigits, const Asn1DateLocalTime* pThis);
flag Asn1DateLocalTime_uper_decode(BitStream* pBitStrm, int fractionDigits, Asn1DateLocalTime* pThis);

flag Asn1DateUtcTime_IsValid(int fractionDigits, Asn1DateUtcTime* pThis);
int Asn1DateUtcTime__compareTo(int fractionDigits, Asn1DateUtcTime* pThis, Asn1DateUtcTime* pOther);
void Asn1DateUtcTime_uper_encode(BitStream* pBitStrm, int fractionDigits, const Asn1DateUtcTime* pThis);
flag Asn1DateUtcTime_uper_decode(BitStream* pBitStrm, int fractionDigits, Asn1DateUtcTime* pThis);

flag Asn1DateTimeWithTimeZone_IsValid(int fractionDigits, Asn1DateTimeWithTimeZone* pThis);
int Asn1DateTimeWithTimeZone_compareTo(int fractionDigits, Asn1DateTimeWithTimeZone* pThis, Asn1DateTimeWithTimeZone* pOther);
void Asn1DateTimeWithTimeZone_uper_encode(BitStream* pBitStrm, int fractionDigits, const Asn1DateTimeWithTimeZone* pThis);
flag Asn1DateTimeWithTimeZone_uper_decode(BitStream* pBitStrm, int fractionDigits, Asn1DateTimeWithTimeZone* pThis);





```
