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

#include "PacketLengthTests.h"

/// \file  PacketLengthTests.c
/// \brief PacketLength unit test suite implementation.

#include "CMocka.h"
#include "TestHelper.h"

#include "../PacketLength.h"
#include "../Errors.h"

enum { MaxTestPacketLength = 1024 };

static bool updateLength(BitStream* stream, int* errCode)
{
  return Asn1PuscLib_PacketLength_update(stream, MaxTestPacketLength, errCode);
}

static bool validateLength(BitStream* stream, int* errCode)
{
  return Asn1PuscLib_PacketLength_validate(stream, MaxTestPacketLength, errCode);
}

/// \Given short message
/// \When updating length
/// \Then packet header is updated
static void PacketLength_update_modifiesHeaderOfShortMessages(void** state)
{
  byte data[] = { EMPTY_PACKET_HEADER, 0x01, 0x02, 0x03 };
  const byte expected[] = { PACKET_HEADER(0x00, 0x02), 0x01, 0x02, 0x03 };

  TEST_CALL(updateLength, data, expected, true, 0);
}

/// \Given long message
/// \When updating length
/// \Then packet header is updated
static void PacketLength_update_modifiesHeaderOfLongMessages(void** state)
{
  byte data[MaxTestPacketLength] = { EMPTY_PACKET_HEADER, 0x01, 0x02, 0x03, 0x42 };
  const byte expected[MaxTestPacketLength] = { PACKET_HEADER(0x03, 0xf9), 0x01, 0x02, 0x03, 0x42 };

  TEST_CALL(updateLength, data, expected, true, 0);
}

/// \Given too short message
/// \When updating length
/// \Then operation fails
static void PacketLength_update_failsForTooShortMessage(void** state)
{
  byte data[] = { EMPTY_PACKET_HEADER };
  const byte expected[] = { EMPTY_PACKET_HEADER };

  TEST_CALL(updateLength, data, expected, false, Asn1PuscLib_ErrorCodes_MessageTooShort);
}

/// \Given too long message
/// \When updating length
/// \Then operation fails
static void PacketLength_update_failsForTooLongMessage(void** state)
{
  byte data[MaxTestPacketLength + 1] = { EMPTY_PACKET_HEADER, 0x01 };
  const byte expected[MaxTestPacketLength + 1] = { EMPTY_PACKET_HEADER, 0x01 };

  TEST_CALL(updateLength, data, expected, false, Asn1PuscLib_ErrorCodes_MessageTooLong);
}

/// \Given too short message
/// \When validating length
/// \Then operation fails
static void PacketLength_validate_failsForTooShortMessage(void** state)
{
  byte data[] = { 0x00, 0x00 };
  INIT_STREAM(data);
  int errCode = 0;

  assert_false(validateLength(&stream, &errCode));
  assert_int_equal(errCode, Asn1PuscLib_ErrorCodes_MessageTooShort);
}

/// \Given too long message
/// \When validating length
/// \Then operation fails
static void PacketLength_validate_failsForTooLongMessage(void** state)
{
  byte data[MaxTestPacketLength + 1] = { PACKET_HEADER(0x03, 0xfa) };
  INIT_STREAM(data);
  int errCode = 0;

  assert_false(validateLength(&stream, &errCode));
  assert_int_equal(errCode, Asn1PuscLib_ErrorCodes_MessageTooLong);
}

/// \Given message with incorrect length
/// \When validating length
/// \Then operation fails
static void PacketLength_validate_failsForMessageWithIncorrectLength(void** state)
{
  byte data[] = { PACKET_HEADER(0x03, 0xf9), 0x01, 0x02 };
  INIT_STREAM(data);
  int errCode = 0;

  assert_false(validateLength(&stream, &errCode));
  assert_int_equal(errCode, Asn1PuscLib_ErrorCodes_BadLength);
}

/// \Given message with correct length
/// \When validating length
/// \Then operation succedes
static void PacketLength_validate_succedesForMessageWithCorrectLength(void** state)
{
  byte data[MaxTestPacketLength] = { PACKET_HEADER(0x03, 0xf9), 0x01, 0x02 };
  INIT_STREAM(data);
  int errCode = 0;

  assert_true(validateLength(&stream, &errCode));
  assert_int_equal(errCode, 0);
}

int Asn1PuscLib_PacketLengthTests_run()
{
  const struct CMUnitTest PacketLengthTests[] = {
    cmocka_unit_test(PacketLength_update_modifiesHeaderOfShortMessages),
    cmocka_unit_test(PacketLength_update_failsForTooShortMessage),
    cmocka_unit_test(PacketLength_update_modifiesHeaderOfLongMessages),
    cmocka_unit_test(PacketLength_update_failsForTooLongMessage),
    cmocka_unit_test(PacketLength_validate_failsForTooShortMessage),
    cmocka_unit_test(PacketLength_validate_failsForMessageWithIncorrectLength),
    cmocka_unit_test(PacketLength_validate_failsForTooLongMessage),
    cmocka_unit_test(PacketLength_validate_succedesForMessageWithCorrectLength),
  };

  return cmocka_run_group_tests(PacketLengthTests, NULL, NULL);
}
