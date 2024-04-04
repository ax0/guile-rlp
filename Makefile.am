include build-aux/guile.am

SOURCES = rlp.sld

TESTS =				\
	tests/decode.scm	\
	tests/encode.scm

EXTRA_DIST =					\
	README.org				\
	autogen.sh		\
	tests/data.scm

RUN_GUILE_SCRIPT = $(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE) --no-auto-compile --no-debug --r7rs

test-encode:
	$(RUN_GUILE_SCRIPT) tests/encode.scm

test-decode:
	$(RUN_GUILE_SCRIPT) tests/encode.scm

test: test-encode test-decode
