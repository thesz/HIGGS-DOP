
run-test: test test.csv net.txt
	./test

net.txt: train train.csv
	./train

train: train.hs Net.hs Dataset.hs
	ghc -o train -odir objs/train -hidir objs/train --make -O3 train.hs

test: test.hs Net.hs
	ghc -o test -odir objs/test -hidir objs/test --make -O3 test.hs

train.csv test.csv: fullcut

fullcut:
	head -n10000000 HIGGS.csv >train.csv
	tail -n500000 HIGGS.csv >test.csv

smallcut:
	head -n1000000 HIGGS.csv >train.csv
	tail -n50000 HIGGS.csv >test.csv

anew:
	rm -f net.txt
