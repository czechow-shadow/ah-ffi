

all: go show


go: 
	stack clean 
	stack build --fast --profile 
	stack exec ah-ffi -- +RTS -xc -xt -hy -l -p -N1

show:
	hp2ps -e9in -g -c ah-ffi.hp
	evince ah-ffi.ps

