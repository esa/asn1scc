#!/bin/bash
rm -rf examiner/*
#sparkmake || exit 1


for file in $(ls *.adb)
do
    if [ "$file" == "mainprogram.adb" -o "$file" == "adaasn1rtl.adb" ] ; then
	continue
    fi
    #spark -sparklib -output=examiner -conf=gnat.cfg -vcg -dpc -i=spark.idx  -language=2005 -warning_file=IgnoredExaminerWarnings.wrn $file
    spark -sparklib -output=examiner -conf=gnat.cfg -vcg -i=spark.idx  -language=2005 -warning_file=IgnoredExaminerWarnings.wrn $file
	if [ $? -ne 0 -a $? -ne 1 ] ; then
	    exit 1
	fi
done

sparksimp -a -p=4 -victor || exit 1

pogs -d=examiner -i  -o=pogs.sum || exit 1

tail -20 examiner/pogs.sum

V=$(tail -20 examiner/pogs.sum | grep ^Totals: | awk '{print $(NF-1) $NF;}' )
 
if [ "$V" != "00" ] ; then exit 1 ; fi

exit 0