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

/// \file  IsoChecksumTests.c
/// \brief IsoChecksum test suite implementation.

#include "IsoChecksumTests.h"

#include "CMocka.h"

#include "../IsoChecksum.h"

/// \Given data from ECSS-E-70-41C B.2.5 test case 1
/// \When calculating checksum for it
/// \Then returned value is equal to the one provided in standard
static void IsoChecksum_calculate_computesProperlyTestCase1FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t data[] = { 0x00, 0x00 };

  const uint16_t checksum = Asn1PuscLib_IsoChecksum_calculate(data, sizeof(data));

  assert_int_equal(checksum, 0xFFFF);
}

/// \Given data from ECSS-E-70-41C B.2.5 test case 2
/// \When calculating checksum for it
/// \Then returned value is equal to the one provided in standard
static void IsoChecksum_calculate_computesProperlyTestCase2FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t data[] = { 0x00, 0x00, 0x00 };

  const uint16_t checksum = Asn1PuscLib_IsoChecksum_calculate(data, sizeof(data));

  assert_int_equal(checksum, 0xFFFF);
}

/// \Given data from ECSS-E-70-41C B.2.5 test case 3
/// \When calculating checksum for it
/// \Then returned value is equal to the one provided in standard
static void IsoChecksum_calculate_computesProperlyTestCase3FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t data[] = { 0xAB, 0xCD, 0xEF, 0x01 };

  const uint16_t checksum = Asn1PuscLib_IsoChecksum_calculate(data, sizeof(data));

  assert_int_equal(checksum, 0x9CF8);
}

/// \Given data from ECSS-E-70-41C B.2.5 test case 4
/// \When calculating checksum for it
/// \Then returned value is equal to the one provided in standard
static void IsoChecksum_calculate_computesProperlyTestCase4FromEcssE7041(void** state)
{
  (void)state;
  const uint8_t data[] = { 0x14, 0x56, 0xF8, 0x9A, 0x00, 0x01 };

  const uint16_t checksum = Asn1PuscLib_IsoChecksum_calculate(data, sizeof(data));

  assert_int_equal(checksum, 0x24DC);
}

int Asn1PuscLib_IsoChecksumTests_run()
{
  const struct CMUnitTest IsoChecksumTests[] = {
    cmocka_unit_test(IsoChecksum_calculate_computesProperlyTestCase1FromEcssE7041),
    cmocka_unit_test(IsoChecksum_calculate_computesProperlyTestCase2FromEcssE7041),
    cmocka_unit_test(IsoChecksum_calculate_computesProperlyTestCase3FromEcssE7041),
    cmocka_unit_test(IsoChecksum_calculate_computesProperlyTestCase4FromEcssE7041),
  };

  return cmocka_run_group_tests(IsoChecksumTests, NULL, NULL);
}
