

TARGET = Canedo_Navares_Villaflores_PE02.rkt

.PHONY: scheme racket


scheme: $(TARGET)
	scheme --quiet < $<

racket: $(TARGET)
	racket -I r5rs -r $<

