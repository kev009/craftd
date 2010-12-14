ACLOCAL_AMFLAGS = -I m4
SUBDIRS = src htdocs

EXTRA_DIST = compat craftd.conf

# Make a correct default config file
edit = sed \
       -e 's|@datadir[@]|$(datadir)|g' \
       -e 's|@sysconfdir[@]|$(sysconfdir)|g'
craftd.conf.dist: Makefile
	rm -f $@ $@.tmp
	srcdir='' \
	       test -f ./$@.in || srcdir=$(srcdir)/; \
	       $(edit) $${srcdir}$@.in >$@.tmp
	mv $@.tmp $@

craftd.conf.dist: $(srcdir)/craftd.conf.dist.in

crafdconfdir = ${sysconfdir}/craftd/
crafdconf_DATA = craftd.conf.dist motd.conf.dist

# Jansson extras
jsondir = third-party/jansson
EXTRA_DIST += $(jsondir)/LICENSE $(jsondir)/README.rst
