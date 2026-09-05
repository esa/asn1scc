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

#include "ErrorsTests.h"

/// \file  ErrorsTests.c
/// \brief Errors functions unit test suite implementation.

#include "CMocka.h"

#include "../Errors.h"

/// \Given any error code
/// \When returning it using returnError
/// \Then false is always returned
static void returnError_alwaysReturnsFalse(void** state)
{
  (void)state;

  assert_false(Asn1PuscLib_returnError(NULL, 0));
}

/// \Given not null pointer to error code
/// \When passing it to returnError
/// \Then memory contents referenced by pointer are updated with error code
static void returnError_setsErrCodeWhenNotNull(void** state)
{
  (void)state;
  int errCode = 0;

  const Asn1PuscLib_ErrorCodes errorValue = Asn1PuscLib_ErrorCodes_BadLength;
  Asn1PuscLib_returnError(&errCode, errorValue);

  assert_int_equal(errCode, errorValue);
}

int Asn1PuscLib_ErrorsTests_run()
{
  const struct CMUnitTest UtilsTests[] = {
    cmocka_unit_test(returnError_alwaysReturnsFalse),
    cmocka_unit_test(returnError_setsErrCodeWhenNotNull),
  };

  return cmocka_run_group_tests(UtilsTests, NULL, NULL);
}
