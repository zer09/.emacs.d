EMACS ?= emacs
CASK = env --unset INSIDE_EMACS ~/.cask/bin/cask

all: install update

bootstrap:
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

version:
	$(CASK) exec $(EMACS) --version

install:
	$(CASK) install || true

update:
	$(CASK) update || true

clean:
	rm -rf .cask
