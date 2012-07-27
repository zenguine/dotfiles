EXENAME=matcher
PREFIX=/home/jcullen
BINDIR=$(PREFIX)/bin

.PHONY: all
all: $(EXENAME)

$(EXENAME): main.c matcher.c
	$(CC) $(CFLAGS) -O3 -Wall $^ -o $@

.PHONY: install
install: $(EXENAME)
	install -d $(BINDIR)
	install -m 0755 $< $(BINDIR)
