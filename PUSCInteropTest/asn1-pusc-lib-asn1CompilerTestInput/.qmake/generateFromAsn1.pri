######################################################################
# Copyright (C) 2017-2023 N7 Space sp. z o. o.
# Contact: https://n7space.com
#
# This file is part of ASN.1/ACN Plugin for QtCreator.
#
# Plugin was developed under a programme and funded by
# European Space Agency.
#
# This Plugin is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This Plugin is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#######################################################################

defineReplace(filterASN1ACNFiles) {
    allFiles = $${1}

    for(file, allFiles) {
        splitted = $$split(file, ".")
        extension = $$last(splitted)

        equals(extension, asn)|equals(extension, asn1)|equals(extension, acn) {
            fileNames += $${_PRO_FILE_PWD_}/$${file}
        }
    }

    return($$fileNames)
}

defineReplace(createASN1SCCInput) {
    allFiles = $${1}

    for(file, allFiles) {
        fileNames += $$shell_quote($$file)
    }

    return($$fileNames)
}

ASN1ACNFILES = $$filterASN1ACNFiles($${DISTFILES})
ASN1SCCINPUT = $$createASN1SCCInput($${ASN1ACNFILES})

cFromAsn1.target += $${ASN1SCC_C_MAIN_HEADER}
cFromAsn1.commands += $$sprintf($$QMAKE_MKDIR_CMD, $$shell_quote($$ASN1SCC_C_DIR))
cFromAsn1.commands += $$escape_expand(\\n\\t)
cFromAsn1.commands += $$ASN1SCC $${ASN1SCC_C_OPTIONS} -o $$shell_quote($$ASN1SCC_C_DIR) $$ASN1SCCINPUT
cFromAsn1.depends += $$ASN1ACNFILES

adaFromAsn1.target += $${ASN1SCC_ADA_MAIN_HEADER}
adaFromAsn1.commands += $$sprintf($$QMAKE_MKDIR_CMD, $$shell_quote($$ASN1SCC_ADA_DIR))
adaFromAsn1.commands += $$escape_expand(\\n\\t)
adaFromAsn1.commands += $$ASN1SCC $${ASN1SCC_ADA_OPTIONS} -o $$shell_quote($$ASN1SCC_ADA_DIR) $$ASN1SCCINPUT
adaFromAsn1.depends += $$ASN1ACNFILES

icdFromAsn1.target += icdFromAsn1
icdFromAsn1.commands += $$sprintf($$QMAKE_MKDIR_CMD, $$shell_quote($$ASN1SCC_ICD_DIR))
icdFromAsn1.commands += $$escape_expand(\\n\\t)
icdFromAsn1.commands += $$ASN1SCC $${ASN1SCC_ICD_OPTIONS} $$shell_quote($${ASN1SCC_ICD_DIR}/$${ASN1SCC_ICD_FILE}) $$ASN1SCCINPUT
icdFromAsn1.commands += $$escape_expand(\\n\\t)
icdFromAsn1.commands += echo Generated ICD: $$shell_quote($${ASN1SCC_ICD_DIR}/$${ASN1SCC_ICD_FILE})
icdFromAsn1.depends += $$ASN1ACNFILES

cleanGenerated.target += cleanGenerated
cleanGenerated.commands += - $$QMAKE_DEL_TREE $$shell_quote($$ASN1SCC_PRODUCTS_DIR)
clean.depends += cleanGenerated

QMAKE_EXTRA_TARGETS += cFromAsn1 adaFromAsn1 icdFromAsn1 cleanGenerated clean
