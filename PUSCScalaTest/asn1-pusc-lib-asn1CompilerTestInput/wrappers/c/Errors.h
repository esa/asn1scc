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

#ifndef ASN1PUSCLIB_ERRORS_H
#define ASN1PUSCLIB_ERRORS_H

/// \file  Errors.h
/// \brief Module containing error codes reported by Asn1PuscLib functions.

#include <stdbool.h>

#include "asn1pusclib_globals.h"

/// \brief Enumeration listing possible errors reported by various Asn1PuscLib functions.
typedef enum {
  /// \brief Message too short.
  Asn1PuscLib_ErrorCodes_MessageTooShort = 0x61736E01,
  /// \brief Message too long.
  Asn1PuscLib_ErrorCodes_MessageTooLong = 0x61736E02,
  /// \brief Message checksum validation failed.
  Asn1PuscLib_ErrorCodes_BadChecksum = 0x61736E03,
  /// \brief Message length validation failed.
  Asn1PuscLib_ErrorCodes_BadLength = 0x61736E04,
} Asn1PuscLib_ErrorCodes;

/// \brief Simplifies writing functions returning boolean and providing optional error code.
/// \details Example:
/// \code
/// bool fun(int* const errCode)
/// {
///   ...
///   if (errorCondition)
///     return Asn1PuscLib_returnError(errCode, ERROR_CONDITION_CODE);
///   ...
/// }
/// \endcode
/// \param [out] errCode pointer to store error code to, can be \c NULL.
/// \param [in] returnedError error code to be set.
/// \returns always \b false
ASN1PUSCLIB_PUBLIC bool Asn1PuscLib_returnError(int* const errCode,
                                                const Asn1PuscLib_ErrorCodes returnedError);

#endif // ASN1PUSCLIB_ERRORS_H
