/****************************************************************************
**
** Copyright (C) 2017-2023 N7 Space sp. z o. o.
** Contact: http://n7space.com
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

#ifndef ASN1PUSCLIB_ISOCHECKSUM_H
#define ASN1PUSCLIB_ISOCHECKSUM_H

#include <stdint.h>
#include <stddef.h>

#include "asn1pusclib_globals.h"

/// \file  IsoChecksum.h
/// \brief Component responsible for ISO checksum calculation as defined in ECSS-E-70-41C Annex B.2.

/// \brief Calculates ISO checksum for given data block.
///        Checksum is calculated according to as ECSS-E-70-41C Annex B.2.
/// \param [in] data pointer to start of data block, can't be NULL.
/// \param [in] length data block length in bytes
/// \returns calculated checksum
ASN1PUSCLIB_PUBLIC uint16_t Asn1PuscLib_IsoChecksum_calculate(const uint8_t* const data,
                                                              const size_t length);

#endif // ASN1PUSCLIB_ISOCHECKSUM_H
