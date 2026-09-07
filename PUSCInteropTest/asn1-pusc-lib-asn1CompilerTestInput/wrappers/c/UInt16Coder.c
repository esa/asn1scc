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

#include "UInt16Coder.h"

#include <stddef.h>

#include <BasicTypes.h>

void Asn1PuscLib_UInt16Coder_encode(const BitStream* const originalStream,
                                    const long offset,
                                    const uint16_t value_)
{
  const TPUSC_UINT16 value = value_;
  BitStream auxStream;
  BitStream_Init(&auxStream,
                 originalStream->buf + offset,
                 TPUSC_UINT16_REQUIRED_BYTES_FOR_ACN_ENCODING);
  TPUSC_UINT16_ACN_Encode(&value, &auxStream, NULL, FALSE);
}

uint16_t Asn1PuscLib_UInt16Coder_decode(const BitStream* const originalStream, const long offset)
{
  BitStream auxStream;
  BitStream_AttachBuffer(&auxStream, originalStream->buf, originalStream->count);
  auxStream.currentByte = offset;

  TPUSC_UINT16 value;
  int ignoredErrCode;
  TPUSC_UINT16_ACN_Decode(&value, &auxStream, &ignoredErrCode);
  return (uint16_t)value;
}
