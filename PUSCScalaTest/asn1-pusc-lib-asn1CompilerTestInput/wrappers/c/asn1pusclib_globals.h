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

#ifndef ASN1PUSCLIB_GLOBALS_H
#define ASN1PUSCLIB_GLOBALS_H

#ifdef __cplusplus
 #define ASN1PUSCLIB_PUBLIC extern "C"
#else
 #if defined(__STDC_VERSION__) && __STDC_VERSION__ < 199901L
   #error "ASN.1 PUS-C Library C wrappers require at least C99"
 #endif
 #define ASN1PUSCLIB_PUBLIC
#endif

#endif // ASN1PUSCLIB_CRC_H
