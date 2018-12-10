outdir = ./out
asn = ./dataview-uniq.asn

mytest: test.c asnlib
	gcc test.c -o mytest ${outdir}/dataview-uniq.c ${outdir}/xer.c ${outdir}/asn1crt.c ${outdir}/real.c

${outdir}:
	mkdir ${outdir}

clean:
	rm -rf ${outdir}
	rm -f mytest

asnlib: ${asn} ${outdir} 
	asn1.exe -XER -c -o ${outdir} ${asn}
