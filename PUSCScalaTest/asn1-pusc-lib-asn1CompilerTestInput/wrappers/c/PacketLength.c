/****************************************************************************
**
** Copyright (C) 2017-2023 N7 Space sp. z o. o.
** Contact: https://n7space.com
**
** This file is part of ASN.1/ACN PUS-C Components Library.
**
** Library was developed under a programme and funded by
** European Space Agency.
**
** This Library is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** This Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program.  If not, see <http://www.gnu.org/licenses/>.
**
****************************************************************************/
#include "PacketLength.h"

#include "Errors.h"
#include "UInt16Coder.h"

enum CcsdsConstants {
  PrimaryHeaderSize = 6,
  PacketDataLengthOffset = 4,
  MaxAllowedPacketDataSize = 65536,
};

static size_t calculatePacketLengthValue(const size_t completePacketLength)
{
  return completePacketLength
          - 1 // As required by standard
          - PrimaryHeaderSize;
}

static bool checkMinimumPacketLength(const size_t length, int* const errCode)
{
  if (length <= PrimaryHeaderSize)
    return Asn1PuscLib_returnError(errCode, Asn1PuscLib_ErrorCodes_MessageTooShort);
  return true;
}

static bool checkMaximumPacketLength(const size_t length,
                                     const size_t maxLength,
                                     int* const errCode)
{
  if (length > maxLength || length > MaxAllowedPacketDataSize)
    return Asn1PuscLib_returnError(errCode, Asn1PuscLib_ErrorCodes_MessageTooLong);
  return true;
}

static void encodeLength(BitStream* const originalStream, const size_t length)
{
  Asn1PuscLib_UInt16Coder_encode(originalStream,
                                 PacketDataLengthOffset,
                                 (uint16_t)length);
}

static size_t decodeLength(const BitStream* const stream)
{
  return Asn1PuscLib_UInt16Coder_decode(stream, PacketDataLengthOffset);
}

static bool checkPacketLengthConstraints(const size_t completeLength,
                                         const size_t maxPacketLength,
                                         int* const errCode)
{
  return checkMinimumPacketLength(completeLength, errCode)
         && checkMaximumPacketLength(completeLength, maxPacketLength, errCode);
}

bool Asn1PuscLib_PacketLength_update(BitStream* const encodedPacket,
                                     const size_t maxPacketLength,
                                     int* const errCode)
{
  const size_t completeLength = (size_t)BitStream_GetLength(encodedPacket);

  if (!checkPacketLengthConstraints(completeLength, maxPacketLength, errCode))
    return false;

  encodeLength(encodedPacket, calculatePacketLengthValue(completeLength));
  return true;
}

bool Asn1PuscLib_PacketLength_validate(const BitStream* const encodedPacket,
                                       const size_t maxPacketLength,
                                       int* const errCode)
{
  const size_t completeLength = (size_t)encodedPacket->currentByte;

  if (!checkPacketLengthConstraints(completeLength, maxPacketLength, errCode))
    return false;

  if (decodeLength(encodedPacket) != calculatePacketLengthValue(completeLength))
    return Asn1PuscLib_returnError(errCode, Asn1PuscLib_ErrorCodes_BadLength);
  return true;
}
