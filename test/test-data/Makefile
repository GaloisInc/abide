# To build via nix: $ nix-shell -p pkgs.gcc-arm-embedded --run make -f Makefile.arm

CCPPC=powerpc64-linux-gnu-gcc
CCX86=gcc

ppcexe = $(patsubst %.c,%.ppc.exe,$(wildcard *.c))
x86exe = $(patsubst %.c,%.x86.exe,$(wildcard *.c))

CFLAGS = -O0 -static

all: $(ppcexe) $(x86exe)

$(ppcexe): %.ppc.exe: %.ppc.s
	$(CCPPC) $(CFLAGS) $< -o $@

%.ppc.s: %.c
	$(CCPPC) $(CFLAGS) -S -c $< -o $@

# NOTE: the PIE builds cannot be verified right now.  It looks like something in
# macaw is choking and not discovering the expected code for PIE binaries
$(x86exe): %.x86.exe: %.x86.s
	$(CCX86) $(CFLAGS) $< -o $@

%.x86.s: %.c
	$(CCX86) $(CFLAGS) -S -c $< -o $@

.PRECIOUS: %.s

clean:
	rm -f *.ppc.s *.ppc.exe *.x86.s *.x86.exe
