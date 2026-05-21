"""
ASN.1 Python Runtime Library - Core Types

This module provides sized integer types and ASN.1 semantic types
that match the behavior of the C and Scala runtime libraries.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Self

from .asn1_exceptions import *
from .encoder import Encoder
from .decoder import Decoder

@dataclass(frozen=True)
class Asn1ConstraintValidResult:
    is_valid: bool
    error_code: int = 0
    message: str = ""

    def __bool__(self) -> bool:
        return self.is_valid

    def __post_init__(self) -> None:
        if not self.is_valid and self.error_code <= 0:
            raise Exception("Error code must be set to a number > 0 if the constraint is not valid.")

        if self.is_valid and self.error_code > 0:
            raise Exception("No error code must be set if the constraint is valid.")

class Asn1Base(ABC):
    
    # def encode(encoding: Encoding) -> bytearray:
    #     pass
    
    # @classmethod
    # def decode(cls, encoding: Encoding, data: bytearray) -> Self:
    #     Decoder.from
    
    @abstractmethod
    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        pass


# Integer types using ctypes for automatic range validation and conversion

# ASN.1 Boolean type - matches primitive bool in C and Scala
class Asn1Boolean(Asn1Base):
    """
    ASN.1 Boolean wrapper that behaves as closely as possible to Python's bool.
    """

    __slots__ = ("_val",)

    def __init__(self, val):
        self._val = bool(val)

    # --- Core protocol ---
    def __bool__(self) -> bool:
        return self._val

    def __repr__(self) -> str:
        return f"Asn1Boolean({self._val})"

    def __str__(self) -> str:
        return str(self._val)

    # --- Equality / ordering ---
    def __eq__(self, other) -> bool:
        return self._val == bool(other)
    
    def __ne__(self, other) -> bool:
        return not self.__eq__(other)

    def __lt__(self, other) -> bool:
        return self._val < bool(other)
    
    def __le__(self, other) -> bool:
        return self._val <= bool(other)
    
    def __gt__(self, other) -> bool:
        return self._val > bool(other)
    
    def __ge__(self, other) -> bool:
        return self._val >= bool(other)

    def __hash__(self) -> int:
        return hash(self._val)

    # --- Boolean operators ---
    def __and__(self, other) -> 'Asn1Boolean':
        return Asn1Boolean(self._val & bool(other))

    def __or__(self, other) -> 'Asn1Boolean':
        return Asn1Boolean(self._val | bool(other))

    def __xor__(self, other) -> 'Asn1Boolean':
        return Asn1Boolean(self._val ^ bool(other))

    def __invert__(self) -> 'Asn1Boolean':
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

import struct as _struct

class Real32(float):
    """Float subclass that rounds to single (IEEE-754 binary32) precision on construction.
    Used as the base class for ASN.1 REAL types when the target word size is 4 bytes,
    mirroring C's behaviour where `asn1Real` is `float` (not `double`)."""
    def __new__(cls, value=0.0):
        rounded = _struct.unpack('f', _struct.pack('f', float(value)))[0]
        return super().__new__(cls, rounded)


class NullType(Asn1Base):
    """
    ASN.1 NullType wrapper that behaves as closely as possible to Python's None.
    Always falsy, always equal to None, singleton instance.
    """

    __slots__ = ()

    # --- Core protocol ---
    def __bool__(self) -> bool:
        return False

    def __repr__(self) -> str:
        return "None"

    def __str__(self) -> str:
        return "None"

    # --- Equality ---
    def __eq__(self, other) -> bool:
        return other is None or isinstance(other, NullType)

    def __ne__(self, other) -> bool:
        return not self.__eq__(other)

    def __hash__(self) -> int:
        return hash(None)

    # --- Pickling / copy compatibility ---
    def __reduce__(self):
        return (NullType, ())

    # --- Prevent accidental mutation / attributes ---
    def __setattr__(self, name, value) -> None:
        raise AttributeError(f"'{self.__class__.__name__}' object has no attributes")

    def __delattr__(self, name) -> None:
        raise AttributeError(f"'{self.__class__.__name__}' object has no attributes")
    
    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        # todo: evaluate if NullType.is_constraint_valid should return True or False
        return Asn1ConstraintValidResult(is_valid=True)
    
    def encode(self, codec: Encoder, check_constraints: bool = True):
        return
    
    @classmethod
    def decode(cls, codec: Decoder, check_constraints: bool = True):
        return NullType()

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


OBJECT_IDENTIFIER_MAX_LENGTH = 20


class Asn1ObjectIdentifier(Asn1Base):
    """
    ASN.1 OBJECT IDENTIFIER type.

    Stores arcs in a fixed-length values list (OBJECT_IDENTIFIER_MAX_LENGTH) plus a
    count of used arcs, matching the C / Scala struct layout so the mutation-style
    generated init code works:

        tc_data.nCount = 3
        tc_data.values[0] = 1
        tc_data.values[1] = 2
        tc_data.values[2] = 3
    """

    def __init__(self, n_count: int = 0, values=None):
        self.nCount: int = n_count
        self.values: list = list(values) if values is not None else [0] * OBJECT_IDENTIFIER_MAX_LENGTH
        if len(self.values) < OBJECT_IDENTIFIER_MAX_LENGTH:
            self.values += [0] * (OBJECT_IDENTIFIER_MAX_LENGTH - len(self.values))

    def is_structurally_valid(self) -> bool:
        """Basic OID structural rules: at least 2 arcs, first ≤ 2, second ≤ 39."""
        return (self.nCount >= 2) and (self.values[0] <= 2) and (self.values[1] <= 39)

    def is_roid_structurally_valid(self) -> bool:
        """RELATIVE-OID structural rule: at least one arc."""
        return self.nCount > 0

    # --- Equality ---
    def __eq__(self, other) -> bool:
        if not isinstance(other, Asn1ObjectIdentifier):
            return False
        if self.nCount != other.nCount:
            return False
        return all(self.values[i] == other.values[i] for i in range(self.nCount))

    def __ne__(self, other) -> bool:
        return not self.__eq__(other)

    def __hash__(self) -> int:
        return hash(tuple(self.values[:self.nCount]))

    def __repr__(self) -> str:
        return f"Asn1ObjectIdentifier({self.values[:self.nCount]})"

    # --- Asn1Base stub ---
    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        raise NotImplementedError("Subclasses must override is_constraint_valid")


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