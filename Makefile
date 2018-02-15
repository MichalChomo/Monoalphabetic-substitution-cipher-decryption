# FLP functional project subs-cipher
# xchomo01, Michal Chomo

HC = ghc
HFLAGS = -Wall
NAME = subs-cipher

.PHONY: clean

$(NAME):
	$(HC) $(HFLAGS) --make Main.hs -o $@

clean:
	rm -f $(NAME) *.o *.hi
