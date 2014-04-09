.PHONY: default

.SUFFIXES:

CABAL ?= cabal
DIST  ?= dist

SCION_CORE_VERSION := $(shell sed -n -e 's/^[Vv]ersion:[[:space:]]*\([0-9][0-9.]*\)/\1/p' scion-core/scion-core.cabal)

SCION_CORE=

default: test

# default: scion-core/dist/build/libHSscion-core-$(SCION_CORE_VERSION).a
# default: scion-core/dist/setup-config

.PHONY: clean
clean:
	(cd scion-core ; $(CABAL) clean)
	(cd tests ; $(CABAL) clean)

##############################################################################
# scion-core

cabal.sandbox.config:
	$(CABAL) sandbox init

scion-core/cabal.sandbox.config:
	(cd scion-core ; $(CABAL) sandbox init --sandbox=../.cabal-sandbox)

scion-core/dist/setup-config: scion-core/scion-core.cabal
	(cd scion-core; $(CABAL) install --only-dependencies && cabal configure)

SCION_CORE_FILES := $(shell find scion-core -name '*.hs')

scion-core/dist/build/libHSscion-core-$(SCION_CORE_VERSION).a: scion-core/dist/setup-config $(SCION_CORE_FILES)
	(cd scion-core; $(CABAL) build -v)

dist/.scion-core-installed: scion-core/dist/build/libHSscion-core-$(SCION_CORE_VERSION).a
	@echo "=== Installing scion-core ==="
	(cd scion-core; $(CABAL) install -v)
	@mkdir -p dist/
	@touch $@

##############################################################################
# tests

tests/cabal.sandbox.config:
	(cd tests ; $(CABAL) sandbox init --sandbox=../.cabal-sandbox)

tests/dist/setup-config: tests/scion-tests.cabal dist/.scion-core-installed tests/cabal.sandbox.config
	(cd tests; $(CABAL) install --only-dependencies -j && cabal configure)

SCION_CORE_FILES := tests/Main.hs

tests/dist/build/test/test: tests/dist/setup-config $(SCION_CORE_FILES)
	(cd tests; $(CABAL) build -v)


.PHONY: test
test: tests/dist/build/test/test
	./tests/dist/build/test/test
