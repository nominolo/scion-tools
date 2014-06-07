.PHONY: default

.SUFFIXES:

CABAL ?= cabal
DIST  ?= dist

SCION_CORE_VERSION := $(shell sed -n -e 's/^[Vv]ersion:[[:space:]]*\([0-9][0-9.]*\)/\1/p' scion-core/scion-core.cabal)
SCION_IPC_VERSION := $(shell sed -n -e 's/^[Vv]ersion:[[:space:]]*\([0-9][0-9.]*\)/\1/p' scion-ipc/scion-ipc.cabal)

# Set up colours. TODO: Detect colour support first (E.g., build bot might not support them)
CRESET="\\033[0m"
CGREEN="\\033[32m"
CYELLOW="\\033[33m"
CBLUE="\\033[34m"
CBOLD="\\033[1m"

default: test

# default: scion-core/dist/build/libHSscion-core-$(SCION_CORE_VERSION).a
# default: scion-core/dist/setup-config
# default: scion-cabal/dist/build/scion-cabal/scion-cabal

.PHONY: clean sandbox-clean
clean:
	(cd scion-ipc ; $(CABAL) clean)
	(cd scion-core ; $(CABAL) clean)
	(cd scion-cabal ; $(CABAL) clean)
	(cd scion-ghc ; $(CABAL) clean)
	(cd tests ; $(CABAL) clean)
	(rm -rf dist)

sandbox-clean: clean
	find . -name 'cabal.sandbox.config' -delete
	rm -r .cabal-sandbox

##############################################################################
# Sandbox

cabal.sandbox.config: 
	$(CABAL) sandbox init

##############################################################################
# scion-ipc

SCION_IPC=scion-ipc/dist/build/libHSscion-ipc-$(SCION_IPC_VERSION).a

scion-ipc/cabal.sandbox.config: cabal.sandbox.config
	(cd scion-ipc ; $(CABAL) sandbox init --sandbox=../.cabal-sandbox)

scion-ipc/dist/setup-config: scion-ipc/scion-ipc.cabal scion-ipc/cabal.sandbox.config
	@echo "$(CBLUE)=== Configuring $(CBOLD)scion-ipc$(CRESET)$(CBLUE) ===$(CRESET)"
	(cd scion-ipc; $(CABAL) install --only-dependencies && cabal configure --builddir=dist)

SCION_IPC_FILES := $(shell find scion-ipc -name '*.hs')

$(SCION_IPC): scion-ipc/dist/setup-config $(SCION_IPC_FILES)
	@echo "$(CBLUE)=== Building $(CBOLD)scion-ipc$(CRESET)$(CBLUE) ===$(CRESET)"
	(cd scion-ipc; $(CABAL) build --builddir=dist)

dist/.scion-ipc-installed: scion-ipc/dist/build/libHSscion-ipc-$(SCION_IPC_VERSION).a
	@echo "$(CBLUE)=== Registering $(CBOLD)scion-ipc$(CRESET)$(CBLUE) ===$(CRESET)"
	(cd scion-ipc; $(CABAL) register --inplace --builddir=dist)
	@mkdir -p dist/
	@touch $@

##############################################################################
# scion-ghc

SCION_GHC_FILES := $(shell find scion-ghc/src -name '*.hs')
SCION_GHC := scion-ghc/dist/build/scion-ghc/scion-ghc

b:
	@echo "$(SCION_GHC_FILES)"

.PHONY: scion-ghc
scion-ghc: ${SCION_GHC}

scion-ghc/cabal.sandbox.config: cabal.sandbox.config
	(cd scion-ghc ; $(CABAL) sandbox init --sandbox=../.cabal-sandbox)

scion-ghc/dist/setup-config: \
		scion-ghc/cabal.sandbox.config \
		dist/.scion-core-installed \
		scion-ghc/scion-ghc.cabal \
	        $(SCION_IPC_FILES)
	@echo "$(CYELLOW)=== Configuring $(CBOLD)scion-ghc$(CRESET)$(CYELLOW) ===$(CRESET)"
	(cd scion-ghc; $(CABAL) install --only-dependencies; $(CABAL) configure --builddir=dist)

$(SCION_GHC): scion-ghc/dist/setup-config \
	      $(SCION_GHC_FILES)
	@echo "$(CYELLOW)=== Building $(CBOLD)scion-ghc$(CRESET)$(CYELLOW) ===$(CRESET)"
	(cd scion-ghc; $(CABAL) build --builddir=dist)
	@touch $@

##############################################################################
# scion-core

SCION_CORE=scion-core/dist/build/libHSscion-core-$(SCION_CORE_VERSION).a

.PHONY: lib
lib: ${SCION_CORE}

scion-core/cabal.sandbox.config: cabal.sandbox.config
	(cd scion-core ; $(CABAL) sandbox init --sandbox=../.cabal-sandbox)

scion-core/dist/setup-config: scion-core/scion-core.cabal ${SCION_IPC} dist/.scion-ipc-installed scion-core/cabal.sandbox.config ${SCION_GHC}
	@echo "$(CBLUE)=== Configuring $(CBOLD)scion-core$(CRESET)$(CBLUE) ===$(CRESET)"
	(cd scion-core; $(CABAL) install --only-dependencies && cabal configure --builddir=dist)

SCION_CORE_FILES := $(shell find scion-core -name '*.hs')

$(SCION_CORE): scion-core/dist/setup-config $(SCION_CORE_FILES)
	@echo "$(CBLUE)=== Building $(CBOLD)scion-core$(CRESET)$(CBLUE) ===$(CRESET)"
	(cd scion-core; $(CABAL) build --builddir=dist)

dist/.scion-core-installed: scion-core/dist/build/libHSscion-core-$(SCION_CORE_VERSION).a
	@echo "$(CBLUE)=== Registering $(CBOLD)scion-core$(CRESET)$(CBLUE) ===$(CRESET)"
	(cd scion-core; $(CABAL) register --inplace --builddir=dist)
	@mkdir -p dist/
	@touch $@

##############################################################################
# scion-cabal

SCION_CABAL_FILES := $(shell find scion-cabal/src -name '*.hs')
SCION_CABAL := scion-cabal/dist/build/scion-cabal/scion-cabal

.PHONY: scion-cabal
scion-cabal: ${SCION_CABAL}

scion-cabal/cabal.sandbox.config: cabal.sandbox.config
	(cd scion-cabal ; $(CABAL) sandbox init --sandbox=../.cabal-sandbox)

scion-cabal/dist/setup-config: \
		scion-cabal/cabal.sandbox.config \
		dist/.scion-core-installed \
		scion-cabal/scion-cabal.cabal \
	        $(SCION_GHC)
	@echo "$(CYELLOW)=== Configuring $(CBOLD)scion-cabal$(CRESET)$(CYELLOW) ===$(CRESET)"
	(cd scion-cabal; $(CABAL) install --only-dependencies && \
	                 $(CABAL) configure --builddir=dist)

$(SCION_CABAL): \
		scion-cabal/dist/setup-config \
		$(SCION_CABAL_FILES)
	@echo "$(CYELLOW)=== Building $(CBOLD)scion-cabal$(CRESET)$(CYELLOW) ===$(CRESET)"
	(cd scion-cabal; $(CABAL) build --builddir=dist)
	@touch $@

##############################################################################
# tests

.PHONY: test
test: tests/dist/build/test/test
	@echo "$(CGREEN)=== Running $(CBOLD)tests$(CRESET)$(CGREEN) ===$(CRESET)"
	./tests/dist/build/test/test -j1


tests/cabal.sandbox.config: cabal.sandbox.config
	(cd tests ; $(CABAL) sandbox init --sandbox=../.cabal-sandbox)

# Note: We depend on the GHC binary to make sure that its dependencies
# get added to the sandbox first.
tests/dist/setup-config: tests/scion-tests.cabal dist/.scion-core-installed \
			 tests/cabal.sandbox.config $(SCION_GHC)
	@echo "$(CGREEN)=== Configuring $(CBOLD)tests$(CRESET)$(CGREEN) ===$(CRESET)"
	(cd tests; $(CABAL) install --only-dependencies -j && cabal configure)

SCION_TEST_FILES := tests/Main.hs

tests/dist/build/test/test: tests/dist/setup-config $(SCION_TEST_FILES) \
			    $(SCION_GHC) $(SCION_CABAL)
	@echo "$(CGREEN)=== Building $(CBOLD)tests$(CRESET)$(CGREEN) ===$(CRESET)"
	(cd tests; $(CABAL) build --builddir=dist)
	@touch $@

##############################################################################
