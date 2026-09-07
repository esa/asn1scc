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

#include "UInt16CoderTests.h"

/// \file  UInt16CoderTests.c
/// \brief UInt16Coder unit test suite implementation.

#include "CMocka.h"

#include "../UInt16Coder.h"

/// \Given bytes stream
/// \When encoding UInt16 in it
/// \Then bytes are replaced with encoded value
static void Asn1PuscLib_UInt16Coder_encode_replacesBytesInStream(void** state)
{
  (void)state;
  byte data[] = { 0xCC, 0xCC, 0xCC, 0xCC };
  BitStream stream;
  BitStream_AttachBuffer(&stream, data, sizeof(data));

  Asn1PuscLib_UInt16Coder_encode(&stream, 1, 0x101);

  const byte expected[] = { 0xCC, 0x01, 0x01, 0xCC };
  assert_memory_equal(data, expected, sizeof(expected));
}

/// \Given bytes stream
/// \When decoding UInt16
/// \Then expected value is read
static void Asn1PuscLib_UInt16Coder_decode_readsBytesFromStream(void** state)
{
  (void)state;
  byte data[] = { 0xCC, 0xDA, 0xFF, 0xCC };
  BitStream stream;
  BitStream_AttachBuffer(&stream, data, sizeof(data));

  const uint16_t value = Asn1PuscLib_UInt16Coder_decode(&stream, 1);

  assert_int_equal(value, 0xdaff);
}

int Asn1PuscLib_UInt16CoderTests_run()
{
  const struct CMUnitTest UInt16CoderTests[] = {
    cmocka_unit_test(Asn1PuscLib_UInt16Coder_encode_replacesBytesInStream),
    cmocka_unit_test(Asn1PuscLib_UInt16Coder_decode_readsBytesFromStream),
  };

  return cmocka_run_group_tests(UInt16CoderTests, NULL, NULL);
}
