"""
ASN.1 Python Runtime Library - Constraint Verification

This module provides constraint validation functions for ASN.1 types.
"""

from typing import Union, Optional, List, Any, Callable
import re
from .asn1_types import Asn1Exception


class ConstraintError(Asn1Exception):
    """Raised when a constraint validation fails"""
    pass


def validate_integer_constraints(value: int,
                                min_val: Optional[int] = None,
                                max_val: Optional[int] = None) -> bool:
    """
    Validate integer constraints.

    Args:
        value: Integer value to validate
        min_val: Minimum allowed value (inclusive)
        max_val: Maximum allowed value (inclusive)

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    if min_val is not None and value < min_val:
        raise ConstraintError(f"Integer value {value} below minimum {min_val}")

    if max_val is not None and value > max_val:
        raise ConstraintError(f"Integer value {value} above maximum {max_val}")

    return True


def validate_real_constraints(value: float,
                             min_val: Optional[float] = None,
                             max_val: Optional[float] = None) -> bool:
    """
    Validate real number constraints.

    Args:
        value: Real value to validate
        min_val: Minimum allowed value (inclusive)
        max_val: Maximum allowed value (inclusive)

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    if min_val is not None and value < min_val:
        raise ConstraintError(f"Real value {value} below minimum {min_val}")

    if max_val is not None and value > max_val:
        raise ConstraintError(f"Real value {value} above maximum {max_val}")

    return True


def validate_string_constraints(value: str,
                               min_length: Optional[int] = None,
                               max_length: Optional[int] = None,
                               allowed_chars: Optional[str] = None,
                               pattern: Optional[str] = None) -> bool:
    """
    Validate string constraints.

    Args:
        value: String value to validate
        min_length: Minimum string length
        max_length: Maximum string length
        allowed_chars: String containing allowed characters
        pattern: Regular expression pattern to match

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # Length constraints
    if min_length is not None and len(value) < min_length:
        raise ConstraintError(f"String length {len(value)} below minimum {min_length}")

    if max_length is not None and len(value) > max_length:
        raise ConstraintError(f"String length {len(value)} above maximum {max_length}")

    # Character constraints
    if allowed_chars is not None:
        for char in value:
            if char not in allowed_chars:
                raise ConstraintError(f"Character '{char}' not in allowed set")

    # Pattern constraint
    if pattern is not None:
        if not re.match(pattern, value):
            raise ConstraintError(f"String '{value}' does not match pattern '{pattern}'")

    return True


def validate_bit_string_constraints(value: str,
                                   min_length: Optional[int] = None,
                                   max_length: Optional[int] = None,
                                   fixed_length: Optional[int] = None) -> bool:
    """
    Validate bit string constraints.

    Args:
        value: Bit string value (containing only '0' and '1')
        min_length: Minimum bit string length
        max_length: Maximum bit string length
        fixed_length: Fixed bit string length

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # Validate bit string format
    if not all(c in '01' for c in value):
        raise ConstraintError("Bit string must contain only '0' and '1' characters")

    # Fixed length constraint
    if fixed_length is not None:
        if len(value) != fixed_length:
            raise ConstraintError(f"Bit string length {len(value)} != required {fixed_length}")
        return True

    # Length constraints
    if min_length is not None and len(value) < min_length:
        raise ConstraintError(f"Bit string length {len(value)} below minimum {min_length}")

    if max_length is not None and len(value) > max_length:
        raise ConstraintError(f"Bit string length {len(value)} above maximum {max_length}")

    return True


def validate_octet_string_constraints(value: Union[bytes, bytearray],
                                     min_length: Optional[int] = None,
                                     max_length: Optional[int] = None,
                                     fixed_length: Optional[int] = None) -> bool:
    """
    Validate octet string constraints.

    Args:
        value: Octet string value
        min_length: Minimum octet string length
        max_length: Maximum octet string length
        fixed_length: Fixed octet string length

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # Fixed length constraint
    if fixed_length is not None:
        if len(value) != fixed_length:
            raise ConstraintError(f"Octet string length {len(value)} != required {fixed_length}")
        return True

    # Length constraints
    if min_length is not None and len(value) < min_length:
        raise ConstraintError(f"Octet string length {len(value)} below minimum {min_length}")

    if max_length is not None and len(value) > max_length:
        raise ConstraintError(f"Octet string length {len(value)} above maximum {max_length}")

    return True


def validate_sequence_constraints(value: Union[dict, object],
                                 required_fields: Optional[List[str]] = None,
                                 optional_fields: Optional[List[str]] = None) -> bool:
    """
    Validate SEQUENCE constraints.

    Args:
        value: Sequence value (dict or object with attributes)
        required_fields: List of required field names
        optional_fields: List of optional field names

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    if isinstance(value, dict):
        fields = set(value.keys())
    else:
        fields = set(attr for attr in dir(value) if not attr.startswith('_'))

    # Check required fields
    if required_fields is not None:
        for field in required_fields:
            if field not in fields:
                raise ConstraintError(f"Required field '{field}' missing from sequence")

    # Check that all fields are either required or optional
    if required_fields is not None and optional_fields is not None:
        allowed_fields = set(required_fields) | set(optional_fields)
        for field in fields:
            if field not in allowed_fields:
                raise ConstraintError(f"Unknown field '{field}' in sequence")

    return True


def validate_choice_constraints(value: Union[dict, object],
                               allowed_alternatives: List[str]) -> bool:
    """
    Validate CHOICE constraints.

    Args:
        value: Choice value (dict or object)
        allowed_alternatives: List of allowed alternative names

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    if isinstance(value, dict):
        if len(value) != 1:
            raise ConstraintError(f"Choice must have exactly one alternative, got {len(value)}")

        chosen_alternative = next(iter(value.keys()))
    else:
        # For objects, look for the first non-None attribute
        chosen_alternative = None
        for attr in dir(value):
            if not attr.startswith('_') and getattr(value, attr) is not None:
                if chosen_alternative is not None:
                    raise ConstraintError("Choice must have exactly one alternative set")
                chosen_alternative = attr

        if chosen_alternative is None:
            raise ConstraintError("Choice must have one alternative set")

    if chosen_alternative not in allowed_alternatives:
        raise ConstraintError(f"Choice alternative '{chosen_alternative}' not in allowed set")

    return True


def validate_enumerated_constraints(value: Union[int, str],
                                   allowed_values: Union[List[int], List[str]]) -> bool:
    """
    Validate ENUMERATED constraints.

    Args:
        value: Enumerated value
        allowed_values: List of allowed values

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    if value not in allowed_values:
        raise ConstraintError(f"Enumerated value '{value}' not in allowed set")

    return True


def validate_sequence_of_constraints(value: List[Any],
                                    min_size: Optional[int] = None,
                                    max_size: Optional[int] = None,
                                    element_validator: Optional[Callable[[Any], bool]] = None) -> bool:
    """
    Validate SEQUENCE OF constraints.

    Args:
        value: Sequence of values
        min_size: Minimum number of elements
        max_size: Maximum number of elements
        element_validator: Function to validate each element

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # Size constraints
    if min_size is not None and len(value) < min_size:
        raise ConstraintError(f"Sequence size {len(value)} below minimum {min_size}")

    if max_size is not None and len(value) > max_size:
        raise ConstraintError(f"Sequence size {len(value)} above maximum {max_size}")

    # Element validation
    if element_validator is not None:
        for i, element in enumerate(value):
            try:
                element_validator(element)
            except Exception as e:
                raise ConstraintError(f"Element {i} validation failed: {e}")

    return True


def validate_set_of_constraints(value: List[Any],
                               min_size: Optional[int] = None,
                               max_size: Optional[int] = None,
                               element_validator: Optional[Callable[[Any], bool]] = None) -> bool:
    """
    Validate SET OF constraints.

    Args:
        value: Set of values
        min_size: Minimum number of elements
        max_size: Maximum number of elements
        element_validator: Function to validate each element

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # Check for duplicates (SET OF requires unique elements)
    if len(value) != len(set(str(v) for v in value)):
        raise ConstraintError("SET OF cannot contain duplicate elements")

    # Use same validation as SEQUENCE OF
    return validate_sequence_of_constraints(value, min_size, max_size, element_validator)


def validate_object_identifier_constraints(value: List[int]) -> bool:
    """
    Validate OBJECT IDENTIFIER constraints.

    Args:
        value: List of integer arcs

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    if len(value) < 2:
        raise ConstraintError("OBJECT IDENTIFIER must have at least 2 arcs")

    # First arc must be 0, 1, or 2
    if value[0] not in [0, 1, 2]:
        raise ConstraintError(f"First arc must be 0, 1, or 2, got {value[0]}")

    # Second arc constraints based on first arc
    if value[0] in [0, 1] and value[1] >= 40:
        raise ConstraintError(f"Second arc must be < 40 when first arc is {value[0]}")

    # All arcs must be non-negative
    for i, arc in enumerate(value):
        if arc < 0:
            raise ConstraintError(f"Arc {i} must be non-negative, got {arc}")

    return True


def validate_utf8_string_constraints(value: str,
                                    min_length: Optional[int] = None,
                                    max_length: Optional[int] = None) -> bool:
    """
    Validate UTF8String constraints.

    Args:
        value: UTF8 string value
        min_length: Minimum string length in characters
        max_length: Maximum string length in characters

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # Validate UTF-8 encoding
    try:
        value.encode('utf-8')
    except UnicodeEncodeError as e:
        raise ConstraintError(f"Invalid UTF-8 string: {e}")

    # Length constraints
    if min_length is not None and len(value) < min_length:
        raise ConstraintError(f"UTF8String length {len(value)} below minimum {min_length}")

    if max_length is not None and len(value) > max_length:
        raise ConstraintError(f"UTF8String length {len(value)} above maximum {max_length}")

    return True


def validate_numeric_string_constraints(value: str,
                                       min_length: Optional[int] = None,
                                       max_length: Optional[int] = None) -> bool:
    """
    Validate NumericString constraints.

    Args:
        value: Numeric string value
        min_length: Minimum string length
        max_length: Maximum string length

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # NumericString can only contain digits and space
    allowed_chars = "0123456789 "

    return validate_string_constraints(
        value,
        min_length=min_length,
        max_length=max_length,
        allowed_chars=allowed_chars
    )


def validate_printable_string_constraints(value: str,
                                         min_length: Optional[int] = None,
                                         max_length: Optional[int] = None) -> bool:
    """
    Validate PrintableString constraints.

    Args:
        value: Printable string value
        min_length: Minimum string length
        max_length: Maximum string length

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # PrintableString allowed characters
    allowed_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 '()+,-./:=?"

    return validate_string_constraints(
        value,
        min_length=min_length,
        max_length=max_length,
        allowed_chars=allowed_chars
    )


def validate_ia5_string_constraints(value: str,
                                   min_length: Optional[int] = None,
                                   max_length: Optional[int] = None) -> bool:
    """
    Validate IA5String constraints.

    Args:
        value: IA5 string value
        min_length: Minimum string length
        max_length: Maximum string length

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # IA5String can only contain ASCII characters (0-127)
    for char in value:
        if ord(char) > 127:
            raise ConstraintError(f"IA5String contains non-ASCII character '{char}'")

    return validate_string_constraints(
        value,
        min_length=min_length,
        max_length=max_length
    )


def validate_visible_string_constraints(value: str,
                                       min_length: Optional[int] = None,
                                       max_length: Optional[int] = None) -> bool:
    """
    Validate VisibleString constraints.

    Args:
        value: Visible string value
        min_length: Minimum string length
        max_length: Maximum string length

    Returns:
        True if valid

    Raises:
        ConstraintError: If validation fails
    """
    # VisibleString can only contain visible ASCII characters (32-126)
    for char in value:
        if not (32 <= ord(char) <= 126):
            raise ConstraintError(f"VisibleString contains non-visible character '{char}'")

    return validate_string_constraints(
        value,
        min_length=min_length,
        max_length=max_length
    )