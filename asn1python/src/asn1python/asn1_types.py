"""
ASN.1 Python Runtime Library - Core Types

This module provides sized integer types and ASN.1 semantic types
that match the behavior of the C and Scala runtime libraries.
"""

import abc
from codecs import Codec
from dataclasses import dataclass
from enum import Enum
from functools import total_ordering


# Error classes
class Asn1Exception(Exception):
    """Base class for ASN.1 runtime errors"""
    pass

class Asn1ValueOutOfRangeException(Asn1Exception):
    """Exception raised when an ASN.1 value is out of range"""
    pass

class Asn1ValueUnexpectedEndOfDataException(Asn1Exception):
    """Exception raised when an ASN.1 value is out of range"""
    pass

class Asn1InvalidValueException(Asn1Exception):
    """Raised when a value is outside the valid range for a type"""
    pass

class Asn1OverflowException(Asn1Exception):
    """Raised when an arithmetic operation would cause overflow"""
    pass

class Asn1TestcaseError(Asn1Exception):
    """Base Class for Testcase Errors"""
    pass

class Asn1TestcaseEncodeFailedError(Asn1TestcaseError):
    """Raised when the encoding fails in a testcase"""
    pass

class Asn1TestcaseDecodeFailedError(Asn1TestcaseError):
    """Raised when the decoding fails in a testcase"""
    pass

class Asn1TestcaseConstraintFailedError(Asn1TestcaseError):
    """Raised when the constraint validation fails in a testcase"""
    pass

class Asn1TestcaseDifferentResultError(Asn1TestcaseError):
    """Raised when the decoding of the encoded object yields a different result in a testcase"""
    pass

@dataclass(frozen=True)
class Asn1ConstraintValidResult:
    is_valid: bool
    error_code: int = 0
    message: str = ""

    def __bool__(self):
        return self.is_valid

    def __post_init__(self):
        if not self.is_valid and self.error_code <= 0:
            raise Exception("Error code must be set to a number > 0 if the constraint is not valid.")

        if self.is_valid and self.error_code > 0:
            raise Exception("No error code must be set if the constraint is valid.")

class Asn1Base(abc.ABC):
    from .encoder import Encoder
    from .decoder import Decoder

    @abc.abstractmethod
    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        pass

    @abc.abstractmethod
    def encode(self, codec: Encoder, check_constraints: bool = True):
        pass

    @classmethod
    def decode(cls, codec: Decoder, check_constraints: bool = True):
        pass

    @staticmethod
    def decode_pure(codec: Decoder, check_constraints: bool = True):
        pass


# Integer types using ctypes for automatic range validation and conversion

# ASN.1 Boolean type - matches primitive bool in C and Scala
@total_ordering
class Asn1Boolean(Asn1Base):
    """
    ASN.1 Boolean wrapper that behaves as closely as possible to Python's bool.
    """
    from .encoder import Encoder

    __slots__ = ("_val",)

    def __init__(self, val):
        self._val = bool(val)

    # --- Core protocol ---
    def __bool__(self):
        return self._val

    def __repr__(self):
        return f"Asn1Boolean({self._val})"

    def __str__(self):
        return str(self._val)

    # --- Equality / ordering ---
    def __eq__(self, other):
        return self._val == bool(other)

    def __lt__(self, other):
        return self._val < bool(other)

    def __hash__(self):
        return hash(self._val)

    # --- Boolean operators ---
    def __and__(self, other):
        return Asn1Boolean(self._val & bool(other))

    def __or__(self, other):
        return Asn1Boolean(self._val | bool(other))

    def __xor__(self, other):
        return Asn1Boolean(self._val ^ bool(other))

    def __invert__(self):
        return Asn1Boolean(not self._val)

    # --- Attribute delegation (for any method/properties bool has) ---
    def __getattr__(self, name):
        return getattr(self._val, name)

    # --- Conversion helpers ---
    @property
    def value(self) -> bool:
        """Explicit access to the inner bool."""
        return self._val

    # --- Stub-Implementations of Asn1Base Methods ---
    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        raise NotImplementedError()

    def encode(self, codec: Encoder, check_constraints: bool = True):
        raise NotImplementedError()

class NullType(Asn1Base):
    """
    ASN.1 NullType wrapper that behaves as closely as possible to Python's None.
    Always falsy, always equal to None, singleton instance.
    """
    from .encoder import Encoder
    from .decoder import Decoder

    __slots__ = ()

    # --- Core protocol ---
    def __bool__(self):
        return False

    def __repr__(self):
        return "None"

    def __str__(self):
        return "None"

    # --- Equality ---
    def __eq__(self, other):
        return other is None or isinstance(other, NullType)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(None)

    # --- Pickling / copy compatibility ---
    def __reduce__(self):
        return (NullType, ())

    # --- Prevent accidental mutation / attributes ---
    def __setattr__(self, name, value):
        raise AttributeError(f"'{self.__class__.__name__}' object has no attributes")

    def __delattr__(self, name):
        raise AttributeError(f"'{self.__class__.__name__}' object has no attributes")
    
    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        # todo: evaluate if NullType.is_constraint_valid should return True or False
        return Asn1ConstraintValidResult(is_valid=True)
    
    def encode(self, codec: Encoder, check_constraints: bool = True):
        return
    
    @classmethod
    def decode(cls, codec: Decoder, check_constraints: bool = True):
        return NullType()
    
    @staticmethod
    def decode_pure(codec: Decoder, check_constraints: bool = True):
        return NullType()

# Constants to match C and Scala implementations
OBJECT_IDENTIFIER_MAX_LENGTH = 20

# Bit manipulation constants
NO_OF_BITS_IN_BYTE = 8
NO_OF_BITS_IN_SHORT = 16
NO_OF_BITS_IN_INT = 32
NO_OF_BITS_IN_LONG = 64

# Error codes to match C implementation
ERR_INSUFFICIENT_DATA = 101
ERR_INCORRECT_PER_STREAM = 102
ERR_INVALID_CHOICE_ALTERNATIVE = 103
ERR_INVALID_ENUM_VALUE = 104
ERR_INVALID_XML_FILE = 200
ERR_INVALID_BER_FILE = 201
ERR_BER_LENGTH_MISMATCH = 202

# Error codes from Scala implementation
NOT_INITIALIZED_ERR_CODE = 1337
ERR_INVALID_ENUM_VALUE_SCALA = 2805
FAILED_READ_ERR_CODE = 5400

# Utility functions to match C and Scala implementations
# def int2uint(v: int) -> int:
#     """Convert signed integer to unsigned (matches C and Scala function)"""
#     return ctypes.c_uint64(v).value

# def uint2int(v: int, uint_size_in_bytes: int) -> int:
#     """Convert unsigned integer to signed (matches C and Scala function)"""
#     match uint_size_in_bytes:
#         case 1: return ctypes.c_int8(v).value
#         case 2: return ctypes.c_int16(v).value
#         case 4: return ctypes.c_int32(v).value
#         case 8: return ctypes.c_int64(v).value
#         case _: raise ValueError(f"Unsupported size: {uint_size_in_bytes}")

# TODO: OctetString_equal might be required for isvalid_python:394
# def OctetString_equal(...):

# TODO: BitString_equal is required for isvalid_python:402
# def BitString_equal(...):


# Time types
class Asn1TimeZone:
    """ASN.1 timezone information"""
    __slots__ = ('sign', 'hours', 'mins')

    def __init__(self, sign: int, hours: int, mins: int):
        """
        Initialize timezone information.
        
        Args:
            sign: -1 or +1
            hours: Hours offset from UTC
            mins: Minutes offset from UTC
        """
        if sign not in (-1, 1):
            raise Asn1InvalidValueException(f"Timezone sign must be -1 or +1, got {sign}")
        if not (0 <= hours <= 23):
            raise Asn1InvalidValueException(f"Timezone hours must be 0-23, got {hours}")
        if not (0 <= mins <= 59):
            raise Asn1InvalidValueException(f"Timezone minutes must be 0-59, got {mins}")
        
        self.sign = sign
        self.hours = hours
        self.mins = mins

    def __str__(self) -> str:
        sign_str = "+" if self.sign == 1 else "-"
        return f"{sign_str}{self.hours:02d}:{self.mins:02d}"

    def __repr__(self) -> str:
        return f"Asn1TimeZone({self.sign}, {self.hours}, {self.mins})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Asn1TimeZone):
            return self.sign == other.sign and self.hours == other.hours and self.mins == other.mins
        return False


class Asn1Date:
    """ASN.1 DATE type"""
    __slots__ = ('years', 'months', 'days')

    def __init__(self, years: int, months: int, days: int):
        """
        Initialize date.
        
        Args:
            years: Year value
            months: Month value (1-12)
            days: Day value (1-31)
        """
        if not (1 <= months <= 12):
            raise Asn1InvalidValueException(f"Month must be 1-12, got {months}")
        if not (1 <= days <= 31):
            raise Asn1InvalidValueException(f"Day must be 1-31, got {days}")
        
        self.years = years
        self.months = months
        self.days = days

    def __str__(self) -> str:
        return f"{self.years:04d}-{self.months:02d}-{self.days:02d}"

    def __repr__(self) -> str:
        return f"Asn1Date({self.years}, {self.months}, {self.days})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Asn1Date):
            return self.years == other.years and self.months == other.months and self.days == other.days
        return False


class Asn1LocalTime:
    """ASN.1 TIME (local time) type"""
    __slots__ = ('hours', 'mins', 'secs', 'fraction')

    def __init__(self, hours: int, mins: int, secs: int, fraction: int = 0):
        """
        Initialize local time.
        
        Args:
            hours: Hours (0-23)
            mins: Minutes (0-59)
            secs: Seconds (0-59)
            fraction: Fractional seconds (implementation-specific precision)
        """
        if not (0 <= hours <= 23):
            raise Asn1InvalidValueException(f"Hours must be 0-23, got {hours}")
        if not (0 <= mins <= 59):
            raise Asn1InvalidValueException(f"Minutes must be 0-59, got {mins}")
        if not (0 <= secs <= 59):
            raise Asn1InvalidValueException(f"Seconds must be 0-59, got {secs}")
        if fraction < 0:
            raise Asn1InvalidValueException(f"Fraction must be non-negative, got {fraction}")
        
        self.hours = hours
        self.mins = mins
        self.secs = secs
        self.fraction = fraction

    def __str__(self) -> str:
        if self.fraction == 0:
            return f"{self.hours:02d}:{self.mins:02d}:{self.secs:02d}"
        else:
            return f"{self.hours:02d}:{self.mins:02d}:{self.secs:02d}.{self.fraction}"

    def __repr__(self) -> str:
        return f"Asn1LocalTime({self.hours}, {self.mins}, {self.secs}, {self.fraction})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Asn1LocalTime):
            return (self.hours == other.hours and self.mins == other.mins and 
                   self.secs == other.secs and self.fraction == other.fraction)
        return False


class Asn1UtcTime:
    """ASN.1 TIME (UTC time) type"""
    __slots__ = ('hours', 'mins', 'secs', 'fraction')

    def __init__(self, hours: int, mins: int, secs: int, fraction: int = 0):
        """
        Initialize UTC time.
        
        Args:
            hours: Hours (0-23)
            mins: Minutes (0-59)
            secs: Seconds (0-59)
            fraction: Fractional seconds (implementation-specific precision)
        """
        if not (0 <= hours <= 23):
            raise Asn1InvalidValueException(f"Hours must be 0-23, got {hours}")
        if not (0 <= mins <= 59):
            raise Asn1InvalidValueException(f"Minutes must be 0-59, got {mins}")
        if not (0 <= secs <= 59):
            raise Asn1InvalidValueException(f"Seconds must be 0-59, got {secs}")
        if fraction < 0:
            raise Asn1InvalidValueException(f"Fraction must be non-negative, got {fraction}")
        
        self.hours = hours
        self.mins = mins
        self.secs = secs
        self.fraction = fraction

    def __str__(self) -> str:
        if self.fraction == 0:
            return f"{self.hours:02d}:{self.mins:02d}:{self.secs:02d}Z"
        else:
            return f"{self.hours:02d}:{self.mins:02d}:{self.secs:02d}.{self.fraction}Z"

    def __repr__(self) -> str:
        return f"Asn1UtcTime({self.hours}, {self.mins}, {self.secs}, {self.fraction})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Asn1UtcTime):
            return (self.hours == other.hours and self.mins == other.mins and 
                   self.secs == other.secs and self.fraction == other.fraction)
        return False


class Asn1TimeWithTimeZone:
    """ASN.1 TIME (time with time zone) type"""
    __slots__ = ('hours', 'mins', 'secs', 'fraction', 'tz')

    def __init__(self, hours: int, mins: int, secs: int, fraction: int, tz: Asn1TimeZone):
        """
        Initialize time with timezone.
        
        Args:
            hours: Hours (0-23)
            mins: Minutes (0-59)
            secs: Seconds (0-59)
            fraction: Fractional seconds (implementation-specific precision)
            tz: Timezone information
        """
        if not (0 <= hours <= 23):
            raise Asn1InvalidValueException(f"Hours must be 0-23, got {hours}")
        if not (0 <= mins <= 59):
            raise Asn1InvalidValueException(f"Minutes must be 0-59, got {mins}")
        if not (0 <= secs <= 59):
            raise Asn1InvalidValueException(f"Seconds must be 0-59, got {secs}")
        if fraction < 0:
            raise Asn1InvalidValueException(f"Fraction must be non-negative, got {fraction}")
        assert isinstance(tz, Asn1TimeZone)
        
        self.hours = hours
        self.mins = mins
        self.secs = secs
        self.fraction = fraction
        self.tz = tz

    def __str__(self) -> str:
        time_str = f"{self.hours:02d}:{self.mins:02d}:{self.secs:02d}"
        if self.fraction != 0:
            time_str += f".{self.fraction}"
        return time_str + str(self.tz)

    def __repr__(self) -> str:
        return f"Asn1TimeWithTimeZone({self.hours}, {self.mins}, {self.secs}, {self.fraction}, {self.tz!r})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Asn1TimeWithTimeZone):
            return (self.hours == other.hours and self.mins == other.mins and 
                   self.secs == other.secs and self.fraction == other.fraction and 
                   self.tz == other.tz)
        return False


class Asn1DateLocalTime:
    """ASN.1 DATE-TIME (local time) type"""
    __slots__ = ('date', 'time')

    def __init__(self, date: Asn1Date, time: Asn1LocalTime):
        """
        Initialize date-time with local time.
        
        Args:
            date: Date component
            time: Local time component
        """
        assert isinstance(date, Asn1Date)
        assert isinstance(time, Asn1LocalTime)
        
        self.date = date
        self.time = time

    def __str__(self) -> str:
        return f"{self.date}T{self.time}"

    def __repr__(self) -> str:
        return f"Asn1DateLocalTime({self.date!r}, {self.time!r})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Asn1DateLocalTime):
            return self.date == other.date and self.time == other.time
        return False


class Asn1DateUtcTime:
    """ASN.1 DATE-TIME (UTC time) type"""
    __slots__ = ('date', 'time')

    def __init__(self, date: Asn1Date, time: Asn1UtcTime):
        """
        Initialize date-time with UTC time.
        
        Args:
            date: Date component
            time: UTC time component
        """
        assert isinstance(date, Asn1Date)
        assert isinstance(time, Asn1UtcTime)

        self.date = date
        self.time = time

    def __str__(self) -> str:
        return f"{self.date}T{self.time}"

    def __repr__(self) -> str:
        return f"Asn1DateUtcTime({self.date!r}, {self.time!r})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Asn1DateUtcTime):
            return self.date == other.date and self.time == other.time
        return False


class Asn1DateTimeWithTimeZone:
    """ASN.1 DATE-TIME (time with time zone) type"""
    __slots__ = ('date', 'time')

    def __init__(self, date: Asn1Date, time: Asn1TimeWithTimeZone):
        """
        Initialize date-time with timezone.
        
        Args:
            date: Date component
            time: Time with timezone component
        """
        assert isinstance(date, Asn1Date)
        assert isinstance(time, Asn1TimeWithTimeZone)
        
        self.date = date
        self.time = time

    def __str__(self) -> str:
        return f"{self.date}T{self.time}"

    def __repr__(self) -> str:
        return f"Asn1DateTimeWithTimeZone({self.date!r}, {self.time!r})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Asn1DateTimeWithTimeZone):
            return self.date == other.date and self.time == other.time
        return False