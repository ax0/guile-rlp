include build-aux/guile.am

SOURCES = rlp.sld

TESTS =				\
	tests/decode.scm	\
	tests/encode.scm

TESTS_ENVIRONMENT = $(AM_V_GEN)$(top_builddir)/pre-inst-env

EXTRA_DIST =					\
	README.org				\
	autogen.sh				\
	pre-inst-env.in
EXTRA_DIST +=					\
	$(TESTS)				\
	tests/data.scm
