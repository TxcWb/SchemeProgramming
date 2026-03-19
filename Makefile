

TARGET = Canedo_Navares_Villaflores_PE02.rkt

.PHONY: all


all: $(TARGET)
	scheme --quiet < $<

