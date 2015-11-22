all: Crosscut

Crosscut:
	mlton Crosscut.mlb

Test:
	mlton Test.mlb

test: Test
	./Test

profile:
	mlton -profile time Crosscut.mlb

profile-count:
	mlton -profile count -profile-branch true Crosscut.mlb

clean:
	rm -f Crosscut Test

.PHONY: all Crosscut Test test clean
