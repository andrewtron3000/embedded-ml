
HUMLOCK = ../humlock

%.um: %.uml ${HUMLOCK}
	time ${HUMLOCK} @MLton max-heap 300m -- -iloptstop 200 -sequence-unit $<;
