.PHONY: default

.SUFFIXES:

CABAL ?= cabal
DIST  ?= dist

SCION_CORE_VERSION := $(shell sed -n -e 's/^[Vv]ersion:[[:space:]]*\([0-9][0-9.]*\)/\1/p' scion-core/scion-core.cabal)

SCION_CORE=

default: scion-core/dist/build/libHSscion-core-$(SCION_CORE_VERSION).a

# default: scion-core/dist/setup-config

.PHONY: clean
clean:
	(cd scion-core ; $(CABAL) clean)

cabal.sandbox.config:
	$(CABAL) sandbox init

scion-core/cabal.sandbox.config:
	(cd scion-core ; $(CABAL) sandbox init --sandbox=../.cabal-sandbox)

scion-core/dist/setup-config: scion-core/scion-core.cabal
	(cd scion-core; $(CABAL) install --only-dependencies && cabal configure)

SCION_CORE_FILES := $(shell find scion-core -name '*.hs')

scion-core/dist/build/libHSscion-core-$(SCION_CORE_VERSION).a: scion-core/dist/setup-config $(SCION_CORE_FILES)
	(cd scion-core; $(CABAL) build -v)
