from dataclasses import dataclass
import abc

class Asn1Base(abc.ABC):
    @abc.abstractmethod
    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        raise NotImplementedException()

    @abc.abstractmethod
    def encode(self, codec: Codec, check_constraints: bool = True):
        raise NotImplementedException()

    @classmethod
    def decode(cls, codec: Codec, check_constraints: bool = True):
        raise NotImplementedException()
    
    @staticmethod
    def decode_pure(codec: Codec, check_constraints: bool = True):
        raise NotImplementedException()

@dataclass(frozen=True)
class Asn1ConstraintValidResult:
    is_valid: bool
    error_code: int = 0

    def __bool__(self):
        return self.is_valid

    def __post_init__(self):
        if not self.is_valid and self.error_code <= 0:
            raise Exception("Error code must be set to a number > 0 if the constraint is not valid.")

        if self.is_valid and self.error_code > 0:
            raise Exception("No error code must be set if the constraint is valid.")
