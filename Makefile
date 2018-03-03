# FLP functional project subs-cipher
# xchomo01, Michal Chomo

HC = ghc
HFLAGS = -Wall
NAME = subs-cipher

.PHONY: all clean

all:
	$(HC) $(HFLAGS) --make Main.hs -o $(NAME)

clean:
	rm -f $(NAME) *.o *.hi
