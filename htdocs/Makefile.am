htdocsdir = $(datadir)/craftd/htdocs
dist_htdocs_DATA = LICENSES.txt
dist_noinst_DATA = js css images index.html schedule.html settings.html
dist_noinst_DATA += logs.html help.html players_blacklist.html players.html

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
