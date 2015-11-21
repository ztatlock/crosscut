all: Crosscut

Crosscut:
	mlton Crosscut.mlb

Test:
	mlton Test.mlb

test: Test
	./Test

clean:
	rm -f Crosscut Test

.PHONY: all Crosscut Test test clean
