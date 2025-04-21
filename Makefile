OL=ol
PREFIX=~/.local

maybe_sqlite != $(OL) -e '(if (has? *features* (quote sqlite)) "`pkg-config --cflags --libs sqlite3`" "")'

jsane: jsane.scm
	$(OL) -x c -o - jsane.scm | $(CC) -static -o jsane -x c - $(maybe_sqlite) -lm -lpthread
clean:
	rm -f jsane
install: jsane
	cp -v jsane $(PREFIX)/bin/jsane
uninstall:
	rm -v $(PREFIX)/bin/jsane
