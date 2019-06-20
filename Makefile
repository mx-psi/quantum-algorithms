# Builds the binaries and places them in the `bin` folder
build:
	stack build
	mkdir -p bin/
	stack install --local-bin-path bin/

# Generates the documentation
# IMPORTANT: On Windows the preprocessing script MUST be
#            manually changed for it to work
docs:
	mkdir -p docs/aux
	./quipper/convert_template.sh a src/lib/HandCraftedOracles.hs docs/aux/handcrafted
	./quipper/convert_template.sh a src/lib/Oracle.hs docs/aux/oracle
	./quipper/convert_template.sh a src/apps/diagram/Classical.hs docs/aux/classical
	cp -b docs/aux/handcrafted src/lib/HandCraftedOracles.hs
	cp -b docs/aux/oracle src/lib/Oracle.hs
	cp -b docs/aux/classical src/apps/diagram/Classical.hs
	stack haddock --no-haddock-deps --haddock-arguments --odir=docs
	mv src/apps/diagram/Classical.hs~ src/apps/diagram/Classical.hs
	mv src/lib/HandCraftedOracles.hs~ src/lib/HandCraftedOracles.hs
	mv src/lib/Oracle.hs~ src/lib/Oracle.hs

# Runs the test suite
test:
	stack test

# Installs dependencies
install-deps:
	curl -sSL https://get.haskellstack.org/ | sh

.PHONY: docs build test install-deps
