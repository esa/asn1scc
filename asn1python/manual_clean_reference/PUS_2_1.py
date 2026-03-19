
from asn1python import *
from typing import *
import BasicTypes
from enum import Enum
from dataclasses import dataclass, field

from manual_clean_reference.general import Asn1Base, Asn1ConstraintValidResult
from src.asn1python import Codec

class DeviceAddress_Enum(Enum):
    addr0 = 1

@dataclass(frozen=True)
class DeviceAddress(Asn1Base):
    val: DeviceAddress_Enum = DeviceAddress_Enum.addr0
    
    class ErrorCodes:
        ERR_DEVICEADDRESS: int = 4111
        ERR_ACN_ENCODE_DEVICEADDRESS: int = 4114
        DeviceAddress_REQUIRED_BYTES_FOR_ACN_ENCODING: int = 0
        DeviceAddress_REQUIRED_BITS_FOR_ACN_ENCODING: int = 0
        ERR_ACN_DECODE_DEVICEADDRESS: int = 4115

    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        is_valid = self.val == DeviceAddress_Enum.addr0
        if is_valid:
            return Asn1ConstraintValidResult(is_valid=True)
        return Asn1ConstraintValidResult(is_valid=False, error_code=self.ErrorCodes.ERR_DEVICEADDRESS)

    def encode(self, codec: Codec, check_constraints: bool = True) -> None:
        if check_constraints:
            res = self.is_constraint_valid()
            if not res:
                raise Asn1Exception("Constraint validation failed.")
        codec.encode_integer(self.val.value, 0, 0)

    @classmethod
    def decode(cls, codec: Codec, check_constraints: bool = True) -> 'DeviceAddress':
        int_val = codec.decodeConstrainedPosWholeNumber(0,0)
        v = DeviceAddress_Enum(int_val)
        instance = cls(val=v)
        if check_constraints:
            res = instance.is_constraint_valid()
            if not res:
                raise Asn1Exception("Constraint validation failed. Decoding failed.")
        return instance

    @staticmethod
    def decode_pure(codec: Codec, check_constraints: bool = True) -> Tuple[Codec, 'DeviceAddress']:
        cpy = codec.copy()
        res = DeviceAddress.decode(cpy, check_constraints)
        return cpy, res


@dataclass(frozen=True)
class TC_2_1_DistributeOnOffDeviceCommands_onOffDeviceAddresses:
    nCount: int = 0
    arr: List[DeviceAddress] = field(default_factory=list)

    class ErrorCodes:
        ERR_TC_2_1_DISTRIBUTEONOFFDEVICECOMMANDS_ONOFFDEVICEADDRESSES: int = 4126  # (SIZE(1 .. maxTC-2-1-OnOffDeviceAdressesCount))
        DeviceAddress_REQUIRED_BYTES_FOR_ACN_ENCODING: int = 0
        DeviceAddress_REQUIRED_BITS_FOR_ACN_ENCODING: int = 0

    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        ret = self.nCount <= 63 and self.nCount >= 1        
        if ret:
            for i in range(self.nCount):
                ret = self.arr[i].is_constraint_valid()
                if not ret:
                    break
        else:
            ret = Asn1ConstraintValidResult(is_valid=False, error_code=self.ErrorCodes.ERR_TC_2_1_DISTRIBUTEONOFFDEVICECOMMANDS_ONOFFDEVICEADDRESSES)
        return ret

    def encode(self, codec: Codec, check_constraints: bool = True) -> None:
        if check_constraints:
            ret = self.is_constraint_valid()
            if not ret:
                raise Asn1Exception("Constraint validation failed. Encoding failed.")
        codec.encode_constraint_whole_number(self.nCount, 1, 63)
        for val in self.arr:
            val.encode(codec)

    @classmethod
    def decode(cls, codec: Codec, check_constraints: bool = True) -> 'TC_2_1_DistributeOnOffDeviceCommands_onOffDeviceAddresses':
        nCount_val = codec.decode_constraint_whole_number(1, 63)
        vals = []
        for _ in range(nCount_val):
            vals.append(DeviceAddress.decode(codec, check_constraints))
        instance = cls(nCount=nCount_val, arr=vals)
        if check_constraints:
            ret = instance.is_constraint_valid()
            if not ret:
                raise Asn1Exception("Constraint validation failed. Decoding failed.")
        return instance

    @staticmethod
    def decode_pure(codec: Codec, check_constraints: bool = True) -> Tuple[Codec, 'TC_2_1_DistributeOnOffDeviceCommands_onOffDeviceAddresses']:
        cpy = codec.copy()
        res = TC_2_1_DistributeOnOffDeviceCommands_onOffDeviceAddresses.decode(cpy, check_constraints)
        return cpy, res

@dataclass(frozen=True)
class TC_2_1_DistributeOnOffDeviceCommands:
    onOffDeviceAddresses: TC_2_1_DistributeOnOffDeviceCommands_onOffDeviceAddresses

    class ErrorCodes:
        ERR_TC_2_1_DISTRIBUTEONOFFDEVICECOMMANDS_ONOFFDEVICEADDRESSES_ELM_2: int = 4121
        ERR_TC_2_1_DISTRIBUTEONOFFDEVICECOMMANDS: int = 4131
        TC_2_1_DistributeOnOffDeviceCommands_REQUIRED_BYTES_FOR_ACN_ENCODING: int = 1
        TC_2_1_DistributeOnOffDeviceCommands_REQUIRED_BITS_FOR_ACN_ENCODING: int = 6
        ERR_ACN_DECODE_TC_2_1_DISTRIBUTEONOFFDEVICECOMMANDS: int = 4135
        ERR_ACN_DECODE_TC_2_1_DISTRIBUTEONOFFDEVICECOMMANDS_ONOFFDEVICEADDRESSES: int = 4130
        ERR_ACN_DECODE_TC_2_1_DISTRIBUTEONOFFDEVICECOMMANDS_ONOFFDEVICEADDRESSES_ELM_2: int = 4125
        maxTC_2_1_OnOffDeviceAdressesCount: int = 63

    def is_constraint_valid(self) -> Asn1ConstraintValidResult:
        return self.onOffDeviceAddresses.is_constraint_valid()

    def encode(self, codec: Codec, check_constraints: bool = True) -> None:
        if check_constraints:
            res = self.is_constraint_valid()
            if not res:
                raise Asn1Exception("Constraint validation failed. Encoding failed.")
        self.onOffDeviceAddresses.encode(codec)

    @classmethod
    def decode(cls, codec: Codec, check_constraints: bool = True) -> 'TC_2_1_DistributeOnOffDeviceCommands':
        addrs = TC_2_1_DistributeOnOffDeviceCommands_onOffDeviceAddresses.decode(codec, check_constraints)
        instance = cls(onOffDeviceAddresses=addrs)
        if check_constraints:
            ret = instance.is_constraint_valid()
            if not ret:
                raise Asn1Exception("Constraint validation failed. Decoding failed.")
        return instance

    @staticmethod
    def decode_pure(codec: Codec, check_constraints: bool = True) -> Tuple[Codec, 'TC_2_1_DistributeOnOffDeviceCommands']:
        cpy = codec.copy()
        res = TC_2_1_DistributeOnOffDeviceCommands.decode(cpy, check_constraints)
        return cpy, res
