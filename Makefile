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

file-clean:
	rm -f *.ppm *.gif *.jpg *.png
	rm -f *.mkv *.mp4 *.mov
	rm -f *.log mlmon.out

.PHONY: all Crosscut Test test clean
