# author:	Jacob Xie
# date:	2024/02/23 09:53:16 Friday
# brief:

check-bounds:
	cd bricks && cabal gen-bounds

build-all:
	cabal build all

gen-hie:
	cd bricks && gen-hie > hie.yaml

