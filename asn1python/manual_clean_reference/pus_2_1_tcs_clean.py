
from asn1python import *
from PUS_2_1 import *
from manual_clean_reference.general import Asn1ConstraintValidResult


def DeviceAddress_ACN_enc_dec(pVal: DeviceAddress, filename: str) -> Union[int, Asn1SccError]:
    codec = ACNCodec(int(DeviceAddress.ErrorCodes.REQUIRED_BYTES_FOR_ACN_ENCODING))

    pVal.encode(codec)
    codec.reset_bitstream()

    # Decode value
    decodedPDU = DeviceAddress.decode(codec)

    if pVal != decodedPDU:
        return Asn1ConstraintValidResult(False, 4)

    codec.reset_bitstream()
    with open(f"{filename}.dat", "wb") as fp:
        fp.write(codec.get_bitstream_buffer())

    return decodedPDU


def DeviceAddress_ACN_enc_dec_nice(pVal: DeviceAddress, filename: str) -> Union[int, Asn1SccError]:
    codec = ACNCodec(int(DeviceAddress_REQUIRED_BYTES_FOR_ACN_ENCODING))

    pVal.encode(codec)
    codec.reset_bitstream()
    
    # Decode value
    decodedPDU = DeviceAddress.decode(codec)

    if pVal != decodedPDU:
        # test_cases_python.stg 74
        return Asn1ConstraintValidResult(False, 4)

    codec.reset_bitstream()
    with open(f"{filename}.dat", "wb") as fp:
        fp.write(codec.get_bitstream_buffer())

    return decodedPDU