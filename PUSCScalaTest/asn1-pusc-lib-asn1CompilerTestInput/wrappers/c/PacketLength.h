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

#ifndef ASN1PUSCLIB_PACKETLENGTH_H
#define ASN1PUSCLIB_PACKETLENGTH_H

/// \file  PacketLength.h
/// \brief Module responsible for maintaining Packer Length CCSDS packet field.

#include <stdbool.h>
#include <stddef.h>

#include <asn1crt.h>

#include "asn1pusclib_globals.h"

/// \brief Updates Packet Length field in CCSDS packet.
/// \param [in,out] encodedPacket Packet to update field in.
/// \param [in] maxPacketLength Maximum allowed packet length.
/// \param [out] errCode Possible error code in case of failure (may be NULL).
/// \retval true Update finished successfuly.
/// \retval false Update failed, error code is provided in errCode.
ASN1PUSCLIB_PUBLIC bool Asn1PuscLib_PacketLength_update(BitStream* const encodedPacket,
                                                        const size_t maxPacketLength,
                                                        int* const errCode);

/// \brief Validates Packet Length field in CCSDS packet.
/// \param [in] encodedPacket Packet to validate.
/// \param [in] maxPacketLength Maximum allowed packet length.
/// \param [out] errCode Possible error code in case of failure (may be NULL).
/// \retval true Validation finished successfuly.
/// \retval false Validation failed, error code is provided in errCode.
ASN1PUSCLIB_PUBLIC bool Asn1PuscLib_PacketLength_validate(const BitStream* const encodedPacket,
                                                          const size_t maxPacketLength,
                                                          int* const errCode);

#endif // ASN1PUSCLIB_PACKETLENGTH_H
