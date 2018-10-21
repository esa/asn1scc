all:
	nuget restore
	xbuild /p:TargetFrameworkVersion="v4.5" || exit 1
	chmod +x Asn1f?/bin/Debug/*.exe
	rm -f Asn1f4/bin/Debug/asn1.exe 
	cp -al Asn1f4/bin/Debug/Asn1f4.exe Asn1f4/bin/Debug/asn1.exe 
