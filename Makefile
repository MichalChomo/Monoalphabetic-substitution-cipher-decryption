# FLP functional project subs-cipher
# xchomo01, Michal Chomo

HC = ghc
HFLAGS = -Wall
NAME = subs-cipher

.PHONY: clean

%.o: %.hs
	$(HC) $(HFLAGS) -c $^

$(NAME): *.o
	$(HC) $(HFLAGS) $^ -o $@

clean:
	rm -f $(NAME) *.o *.hi
