hc = ghc
hcf = -dynamic

a.out : ExtendedUtils.hs Main.hs
	$(hc) $(hcf) $^ -o $@

.PHONY : clean
clean : 
	rm -f *.out *.hi *.o
