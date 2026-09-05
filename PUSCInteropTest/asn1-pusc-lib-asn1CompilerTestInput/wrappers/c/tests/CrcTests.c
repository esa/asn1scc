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

#include "CrcTests.h"

/// \file  CrcTests.c
/// \brief Crc tests suites implementation.

#include "CMocka.h"

#include "../Crc.h"

/// \Given data from ECSS-E-70-41C B.1.5 test case 1
/// \When calculating checksum for it
/// \Then returned value is equal to the one provided in standard
static void Crc_calculate_computesProperlyTestCase1FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t data[] = { 0x00, 0x00 };

  const uint16_t crc = Asn1PuscLib_Crc_calculate(data, sizeof(data));

  assert_int_equal(crc, 0x1D0F);
}

/// \Given data from ECSS-E-70-41C B.1.5 test case 2
/// \When calculating checksum for it
/// \Then returned value is equal to the one provided in standard
static void Crc_calculate_computesProperlyTestCase2FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t data[] = { 0x00, 0x00, 0x00 };

  const uint16_t crc = Asn1PuscLib_Crc_calculate(data, sizeof(data));

  assert_int_equal(crc, 0xCC9C);
}

/// \Given data from ECSS-E-70-41C B.1.5 test case 3
/// \When calculating checksum for it
/// \Then returned value is equal to the one provided in standard
static void Crc_calculate_computesProperlyTestCase3FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t data[] = { 0xAB, 0xCD, 0xEF, 0x01 };

  const uint16_t crc = Asn1PuscLib_Crc_calculate(data, sizeof(data));

  assert_int_equal(crc, 0x04A2);
}

/// \Given data from ECSS-E-70-41C B.1.5 test case 4
/// \When calculating checksum for it
/// \Then returned value is equal to the one provided in standard
static void Crc_calculate_computesProperlyTestCase4FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t data[] = { 0x14, 0x56, 0xF8, 0x9A, 0x00, 0x01 };

  const uint16_t crc = Asn1PuscLib_Crc_calculate(data, sizeof(data));

  assert_int_equal(crc, 0x7FD5);
}

/// \Given data from ECSS-E-70-41C B.1.5 test case 1 in multiple parts
/// \When calculating checksum for all parts
/// \Then returned value is equal to the one provided in standard
static void Crc_update_computesProperlyTestCase1FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t dataPart1[] = { 0x00 };
  const uint8_t dataPart2[] = { 0x00 };

  const uint16_t crcPart1 = Asn1PuscLib_Crc_calculate(dataPart1, sizeof(dataPart1));
  const uint16_t completeCrc = Asn1PuscLib_Crc_update(crcPart1, dataPart2, sizeof(dataPart2));

  assert_int_equal(completeCrc, 0x1D0F);
}

/// \Given data from ECSS-E-70-41C B.1.5 test case 2 in multiple parts
/// \When calculating checksum for all parts
/// \Then returned value is equal to the one provided in standard
static void Crc_update_computesProperlyTestCase2FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t dataPart1[] = { 0x00 };
  const uint8_t dataPart2[] = { 0x00, 0x00 };

  const uint16_t crcPart1 = Asn1PuscLib_Crc_calculate(dataPart1, sizeof(dataPart1));
  const uint16_t completeCrc = Asn1PuscLib_Crc_update(crcPart1, dataPart2, sizeof(dataPart2));

  assert_int_equal(completeCrc, 0xCC9C);
}

/// \Given data from ECSS-E-70-41C B.1.5 test case 3 in multiple parts
/// \When calculating checksum for all parts
/// \Then returned value is equal to the one provided in standard
static void Crc_update_computesProperlyTestCase3FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t dataPart1[] = { 0xAB, 0xCD, 0xEF };
  const uint8_t dataPart2[] = { 0x01 };

  const uint16_t crcPart1 = Asn1PuscLib_Crc_calculate(dataPart1, sizeof(dataPart1));
  const uint16_t completeCrc = Asn1PuscLib_Crc_update(crcPart1, dataPart2, sizeof(dataPart2));

  assert_int_equal(completeCrc, 0x04A2);
}

/// \Given data from ECSS-E-70-41C B.1.5 test case 4 in multiple parts
/// \When calculating checksum for all parts
/// \Then returned value is equal to the one provided in standard
static void Crc_update_computesProperlyTestCase4FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t dataPart1[] = { 0x14, 0x56 };
  const uint8_t dataPart2[] = { 0xF8, 0x9A, 0x00 };
  const uint8_t dataPart3[] = { 0x01 };

  const uint16_t crcPart1 = Asn1PuscLib_Crc_calculate(dataPart1, sizeof(dataPart1));
  const uint16_t crcPart2 = Asn1PuscLib_Crc_update(crcPart1, dataPart2, sizeof(dataPart2));
  const uint16_t completeCrc = Asn1PuscLib_Crc_update(crcPart2, dataPart3, sizeof(dataPart3));

  assert_int_equal(completeCrc, 0x7FD5);
}

int Asn1PuscLib_CrcTests_run()
{
  const struct CMUnitTest CrcTests[] = {
    cmocka_unit_test(Crc_calculate_computesProperlyTestCase1FromEcssE7041),
    cmocka_unit_test(Crc_calculate_computesProperlyTestCase2FromEcssE7041),
    cmocka_unit_test(Crc_calculate_computesProperlyTestCase3FromEcssE7041),
    cmocka_unit_test(Crc_calculate_computesProperlyTestCase4FromEcssE7041),
    cmocka_unit_test(Crc_update_computesProperlyTestCase1FromEcssE7041),
    cmocka_unit_test(Crc_update_computesProperlyTestCase2FromEcssE7041),
    cmocka_unit_test(Crc_update_computesProperlyTestCase3FromEcssE7041),
    cmocka_unit_test(Crc_update_computesProperlyTestCase4FromEcssE7041),
  };

  return cmocka_run_group_tests(CrcTests, NULL, NULL);
}
