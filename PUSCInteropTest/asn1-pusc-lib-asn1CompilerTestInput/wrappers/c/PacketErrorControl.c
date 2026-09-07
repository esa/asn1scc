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
#include "PacketErrorControl.h"

#include <stddef.h>

#include "Errors.h"
#include "UInt16Coder.h"
#include "IsoChecksum.h"
#include "Crc.h"

enum { PacketErrorControlBytes = 2 };

static size_t packetErrorControlOffset(const BitStream* const packet)
{
  return (size_t)(packet->currentByte - PacketErrorControlBytes);
}

static void encodeChecksum(BitStream* const originalStream, const uint16_t checksum)
{
  Asn1PuscLib_UInt16Coder_encode(originalStream,
                                 packetErrorControlOffset(originalStream),
                                 checksum);
}

static uint16_t decodeChecksum(const BitStream* const originalStream)
{
  return Asn1PuscLib_UInt16Coder_decode(originalStream, packetErrorControlOffset(originalStream));
}

static uint16_t calculateChecksum(const BitStream* const packet,
                                  const Asn1PuscLib_PacketErrorControl_Type type)
{
  const size_t len = packetErrorControlOffset(packet);
  return (type == Asn1PuscLib_PacketErrorControl_Type_Crc)
    ? Asn1PuscLib_Crc_calculate(packet->buf, len)
    : Asn1PuscLib_IsoChecksum_calculate(packet->buf, len);
}

bool Asn1PuscLib_PacketErrorControl_update(BitStream* const packet,
                                           const Asn1PuscLib_PacketErrorControl_Type type,
                                           int* const errCode)
{
  if (packet->currentByte <= PacketErrorControlBytes)
    return Asn1PuscLib_returnError(errCode, Asn1PuscLib_ErrorCodes_MessageTooShort);

  encodeChecksum(packet, calculateChecksum(packet, type));

  return true;
}

bool Asn1PuscLib_PacketErrorControl_validate(const BitStream* const packet,
                                             const Asn1PuscLib_PacketErrorControl_Type type,
                                             int* const errCode)
{
  if (packet->currentByte <= PacketErrorControlBytes)
    return Asn1PuscLib_returnError(errCode, Asn1PuscLib_ErrorCodes_MessageTooShort);

  if (calculateChecksum(packet, type) != decodeChecksum(packet))
    return Asn1PuscLib_returnError(errCode, Asn1PuscLib_ErrorCodes_BadChecksum);

  return true;
}
