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

#include "PacketErrorControlTests.h"

/// \file  PacketErrorControlTests.c
/// \brief PacketErrorControl test suite implementation.

#include "CMocka.h"
#include "TestHelper.h"

#include "../PacketErrorControl.h"
#include "../Errors.h"

static bool updateIso(BitStream* stream, int* errCode)
{
  return Asn1PuscLib_PacketErrorControl_update(stream,
                                               Asn1PuscLib_PacketErrorControl_Type_Iso,
                                               errCode);
}

static bool updateCrc(BitStream* stream, int* errCode)
{
  return Asn1PuscLib_PacketErrorControl_update(stream,
                                               Asn1PuscLib_PacketErrorControl_Type_Crc,
                                               errCode);
}

static bool validateIso(const BitStream* stream, int* errCode)
{
  return Asn1PuscLib_PacketErrorControl_validate(stream,
                                                 Asn1PuscLib_PacketErrorControl_Type_Iso,
                                                 errCode);
}

static bool validateCrc(const BitStream* stream, int* errCode)
{
  return Asn1PuscLib_PacketErrorControl_validate(stream,
                                                 Asn1PuscLib_PacketErrorControl_Type_Crc,
                                                 errCode);
}

/// \Given message
/// \When updating checksum using ISO method
/// \Then mesages last bytes are updated
static void PacketErrorControl_update_modifiesLastTwoBytesOfMessageUsingIso(void** state)
{
  byte data[10] = { 0x00 };
  const byte expected[10] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF };

  TEST_CALL(updateIso, data, expected, true, 0);
}

/// \Given message
/// \When updating checksum using CRC method
/// \Then mesages last bytes are updated
static void PacketErrorControl_update_modifiesLastTwoBytesOfMessageUsingCrc(void** state)
{
  byte data[10] = { 0x00 };
  const byte expected[10] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x31, 0x3E };

  TEST_CALL(updateCrc, data, expected, true, 0);
}

/// \Given too short message
/// \When updating checksum
/// \Then operation fails
static void PacketErrorControl_update_failsForTooShortMessage(void** state)
{
  byte data[2] = { 0x00 };
  const byte expected[2] = { 0x00 };

  TEST_CALL(updateIso, data, expected, false, Asn1PuscLib_ErrorCodes_MessageTooShort);
}

/// \Given too short message
/// \When validating checksum
/// \Then operation fails
static void PacketErrorControl_validate_failsForTooShortMessage(void** state)
{
  byte data[2] = { 0x00 };

  TEST_CALL(validateIso, data, data, false, Asn1PuscLib_ErrorCodes_MessageTooShort);
}

/// \Given message with incorrect checksum
/// \When validating checksum
/// \Then operation fails
static void PacketErrorControl_validate_failsForBadChecksum(void** state)
{
  byte data[4] = { 0x00 };

  TEST_CALL(validateCrc, data, data, false, Asn1PuscLib_ErrorCodes_BadChecksum);
}

/// \Given message with correct ISO checksum
/// \When validating checksum
/// \Then operation succedes
static void PacketErrorControl_validate_succedesForCorrectIsoChecksum(void** state)
{
  byte data[] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF };

  TEST_CALL(validateIso, data, data, true, 0);
}

/// \Given message with correct CRC checksum
/// \When validating checksum
/// \Then operation succedes
static void PacketErrorControl_validate_succedesForCorrectCrcChecksum(void** state)
{
  byte data[] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x31, 0x3E };

  TEST_CALL(validateCrc, data, data, true, 0);
}

int Asn1PuscLib_PacketErrorControlTests_run()
{
  const struct CMUnitTest PacketErrorControlTests[] = {
    cmocka_unit_test(PacketErrorControl_update_modifiesLastTwoBytesOfMessageUsingIso),
    cmocka_unit_test(PacketErrorControl_update_modifiesLastTwoBytesOfMessageUsingCrc),
    cmocka_unit_test(PacketErrorControl_update_failsForTooShortMessage),
    cmocka_unit_test(PacketErrorControl_validate_failsForTooShortMessage),
    cmocka_unit_test(PacketErrorControl_validate_failsForBadChecksum),
    cmocka_unit_test(PacketErrorControl_validate_succedesForCorrectIsoChecksum),
    cmocka_unit_test(PacketErrorControl_validate_succedesForCorrectCrcChecksum),
  };

  return cmocka_run_group_tests(PacketErrorControlTests, NULL, NULL);
}
