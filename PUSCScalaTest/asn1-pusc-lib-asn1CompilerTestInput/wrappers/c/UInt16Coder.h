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

#ifndef ASN1PUSCLIB_UINT16CODER_H
#define ASN1PUSCLIB_UINT16CODER_H

#include "asn1pusclib_globals.h"

/// \file  UInt16Coder.h
/// \brief Module implementing coding of unsigned 16bit int ASN.1/ACN type on stream.
/// \details This is internal module, used by other modules in this component.
///          Should not be used directly.

#include <stdint.h>
#include <asn1crt.h>

/// \brief Encodes given value on stream. Does not modify stream state.
/// \param [in] originalStream Original stream to inject value to.
/// \param [in] offset Offset from stream start on which value should be encoded.
/// \param [in] value Value to encode.
ASN1PUSCLIB_PUBLIC void Asn1PuscLib_UInt16Coder_encode(const BitStream* const originalStream,
                                                       const long offset,
                                                       const uint16_t value);

/// \brief Decodes given value from stream. Does not modify stream state.
/// \param [in] originalStream Stream to decode from.
/// \param [in] offset Offset from stream start from which value should be decoded.
/// \returns Decoded value.
ASN1PUSCLIB_PUBLIC uint16_t Asn1PuscLib_UInt16Coder_decode(const BitStream* const originalStream,
                                                           const long offset);

#endif // ASN1PUSCLIB_UINT16CODER_H
