
from asn1python import *
from typing import *
import BasicTypes
from enum import Enum
from dataclasses import dataclass, field

from src.asn1python import Asn1Exception
from src.asn1python.asn1_types import Asn1Base

import VerificationRequest


@dataclass(frozen=True)
class TM_1_1_SuccessfulAcceptanceVerificationReport(Asn1Base):

    request_ID: VerificationRequest.VerificationRequest_ID

    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        return self.request_ID.is_constraint_valid()

    def encode(self, codec: Codec, check_constraints: bool = True) -> None:
        if check_constraints:
            res = self.is_constraint_valid()
            if not res:
                raise Asn1Exception("Constraint validation failed.")
        self.request_ID.encode(codec, check_constraints)            

    @classmethod
    def decode(cls, codec: Codec, check_constraints: bool = True) -> 'TM_1_1_SuccessfulAcceptanceVerificationReport':
        request_ID = VerificationRequest.VerificationRequest_ID.decode(codec, check_constraints)
        instance = cls(request_ID=request_ID)
        if check_constraints:
            res = instance.is_constraint_valid()
            if not res:
                raise Asn1Exception("Constraint validation failed. Decoding failed.")
        return instance

    @staticmethod
    def decode_pure(codec: Codec, check_constraints: bool = True) -> Tuple[Codec, 'TM_1_1_SuccessfulAcceptanceVerificationReport']:
        cpy = codec.copy()
        res = TM_1_1_SuccessfulAcceptanceVerificationReport.decode(cpy, check_constraints)
        return cpy, res