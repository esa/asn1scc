all: html excel

html:
	../icd-generator.py -t html --root ASW-TELECOMMAND --root ASW-TELEMETRY -o pus.html ccsds-space-packet.asn Flight_Software-ASW.asn foo.asn ccsds-space-packet.acn Flight_Software-ASW.acn foo.acn
	../icd-generator.py -t html -o lidar.html --root Packet lidar.asn lidar.acn

excel:
	../icd-generator.py -t excel --root ASW-TELECOMMAND --root ASW-TELEMETRY -o pus.xlsx ccsds-space-packet.asn Flight_Software-ASW.asn foo.asn ccsds-space-packet.acn Flight_Software-ASW.acn foo.acn
	../icd-generator.py -t excel -o lidar.xlsx --root Packet lidar.asn lidar.acn

clean:
	rm -f pus.html lidar.html pus.xlsx lidar.xlsx
