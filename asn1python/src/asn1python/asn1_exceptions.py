from typing import Optional

# Error classes

class Asn1SccError:
    def __init__(self, error_code: int):
        self.error_code = error_code

    def get_error_code(self) -> int:
        return self.error_code

class Asn1Exception(Exception):
    """Base class for ASN.1 runtime errors"""
    pass

class Asn1ValueOutOfRangeException(Asn1Exception):
    """Exception raised when a decoded or assigned value violates a range constraint"""
    def __init__(self, message: str = "", field_name: Optional[str] = None):
        super().__init__(message)
        self.field_name = field_name

class Asn1UnexpectedEndOfDataException(Asn1Exception):
    """Exception raised when the input buffer ends prematurely during decoding"""
    def __init__(self, message: str = "", field_name: Optional[str] = None):
        super().__init__(message)
        self.field_name = field_name

class Asn1InvalidValueException(Asn1Exception):
    """Exception raised when a decoded value is structurally invalid (e.g. unknown enum index, invalid choice discriminant)"""
    def __init__(self, message: str = "", field_name: Optional[str] = None):
        super().__init__(message)
        self.field_name = field_name

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