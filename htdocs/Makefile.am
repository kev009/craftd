htdocsdir = $(datadir)/craftd/htdocs
dist_htdocs_DATA = LICENSES.txt
dist_noinst_DATA = index.html jquery-1.4.4.min.js

install-data-hook:
	if ! test -d $(htdocsdir); then \
		$(mkdir_p) $(htdocsdir); \
		for p in $(dist_noinst_DATA); do \
			if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
			echo "$$d$$p"; \
		done | $(am__base_list) | \
		while read files; do \
			echo " $(INSTALL_DATA) $$files '$(htdocsdir)'"; \
			$(INSTALL_DATA) $$files "$(htdocsdir)" || exit $$?; \
		done \
	fi
