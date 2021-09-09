ASN1SCC=asn1.exe
outdir=out
outfile=${outdir}/Dataview.c

${outdir}:
	mkdir -p ${outdir}

${outfile}: Dataview.asn Dataview.acn ${outdir}
	${ASN1SCC} -c -o ${outdir}/ -ACN Dataview.acn Dataview.asn


check: ${outfile}
	@grep -A 9 "T_Packet_STM13_ACN_Encode" ${outfile}
	@echo "<See above if body is empty>"

clean:
	@rm -rf ${outdir}
