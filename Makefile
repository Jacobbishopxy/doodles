# author:	Jacob Xie
# date:	2024/02/23 09:53:16 Friday
# brief:

check-bounds:
	cd bricks && cabal gen-bounds

build-all:
	cabal build all

gen-hie-bricks:
	cd bricks && gen-hie > hie.yaml

gen-hie-misc:
	cd misc && gen-hie > hie.yaml

gen-hie: gen-hie-bricks gen-hie-misc
