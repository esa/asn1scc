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

#ifndef ASN1PUSCLIB_TESTS_TESTHELPER_H
#define ASN1PUSCLIB_TESTS_TESTHELPER_H

/// \file  TestHelper.h
/// \brief Utility module for unit tests.

/// \cond TestsInternals
#define INIT_EMPTY_STREAM(data)                                                                    \
  BitStream stream;                                                                                \
  (void)state;                                                                                     \
                                                                                                   \
  BitStream_AttachBuffer(&stream, data, sizeof(data));

#define INIT_STREAM(data)                                                                          \
  INIT_EMPTY_STREAM(data)                                                                          \
  stream.currentByte = sizeof(data);

#define COMPARE_RESULTS(expectedErrCode, expectedResult, expectedOutput)                           \
  assert_int_equal(errCode, (expectedErrCode));                                                    \
  assert_int_equal(result, (expectedResult));                                                      \
  assert_memory_equal(output, (expectedOutput), sizeof(data));

#define TEST_CALL(X, data, expectedOutput, expectedResult, expectedErrCode)                        \
  {                                                                                                \
    INIT_STREAM(data);                                                                             \
    const byte* const output = data;                                                               \
    int errCode = 0;                                                                               \
                                                                                                   \
    const bool result = X(&stream, &errCode);                                                      \
                                                                                                   \
    COMPARE_RESULTS(expectedErrCode, expectedResult, expectedOutput);                              \
  }

#define PACKET_HEADER(lenHi, lenLo) 0xAA, 0xAA, 0xAA, 0xAA, lenHi, lenLo
#define EMPTY_PACKET_HEADER PACKET_HEADER(0x00, 0x00)

/// \endcond

#endif // ASN1PUSCLIB_TESTS_TESTHELPER_H
