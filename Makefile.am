ACLOCAL_AMFLAGS = -I m4

SUBDIRS = include src plugins htdocs

EXTRA_DIST = craftd.conf.dist.in motd.conf.dist

# Make a correct default config file
edit = sed \
       -e 's|@localstatedir[@]|$(localstatedir)|g' \
       -e 's|@datdadir[@]|$(datadir)|g' \
       -e 's|@sysconfdir[@]|$(sysconfdir)|g' \
       -e 's|@libdir[@]|$(libdir)|g'
craftd.conf.dist: Makefile
	rm -f $@ $@.tmp
	srcdir='' \
	       test -f ./$@.in || srcdir=$(srcdir)/; \
	       $(edit) $${srcdir}$@.in >$@.tmp
	mv $@.tmp $@

craftd.conf.dist: $(srcdir)/craftd.conf.dist.in

DISTCLEANFILES = craftd.conf.dist

craftdconfdir = ${sysconfdir}/craftd/
craftdconf_DATA = craftd.conf.dist motd.conf.dist

# bstring extas
bstrdir = third-party/bstring
EXTRA_DIST += $(bstrdir)/license.txt $(bstrdir)/security.txt \
	      $(bstrdir)/bstrlib.txt $(bstrdir)/porting.txt

# klib extras
klibdir = third-party/klib
EXTRA_DIST += $(klibdir)

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(localstatedir)/craftd/world
