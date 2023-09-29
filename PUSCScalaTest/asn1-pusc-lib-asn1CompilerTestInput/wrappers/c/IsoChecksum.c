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
#include "IsoChecksum.h"

#define MAKE_UINT16(hi, lo) (uint16_t)((hi) << 8 | (lo))
#define REDUCE(x) (((x)&0xFF) + ((x) >> 8))

static uint16_t fletcher16(const uint8_t* const data, const size_t bytes)
{
  int sum1 = 0xff;
  int sum2 = 0xff;
  size_t bytesToProcess = bytes;
  const uint8_t* it = data;

  while (bytesToProcess > 0)
  {
    size_t tlen = bytesToProcess > 20 ? 20 : bytesToProcess;
    bytesToProcess -= tlen;
    do
    {
      sum1 += *it++;
      sum2 += sum1;
    } while (--tlen);
    sum1 = REDUCE(sum1);
    sum2 = REDUCE(sum2);
  }

  return MAKE_UINT16(REDUCE(sum2), REDUCE(sum1));
}

uint16_t Asn1PuscLib_IsoChecksum_calculate(const uint8_t* const data, const size_t length)
{
  const uint16_t csum = fletcher16(data, length);

  const int f0 = csum & 0xFF;
  const int f1 = (csum >> 8) & 0xFF;

  const int c0 = 0xFF - ((f0 + f1) % 0xFF);
  const int c1 = 0xFF - ((f0 + c0) % 0xFF);

  return MAKE_UINT16(c0, c1);
}
