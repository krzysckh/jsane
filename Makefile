OL=ol
OPT=-O2
PREFIX=~/.local

maybe_sqlite != $(OL) -e "(if (has? *features* 'sqlite) '-lsqlite3 \"\")"

jsane: jsane.scm
	$(OL) $(OPT) -x c -o - jsane.scm | $(CC) -static -o jsane -x c - $(maybe_sqlite) -lm
clean:
	rm -f jsane
install:
	cp -v jsane $(PREFIX)/bin/jsane
uninstall:
	rm -v $(PREFIX)/bin/jsane
