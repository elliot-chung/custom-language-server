UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
ARCH := elf64
endif
ifeq ($(UNAME), Darwin)
ARCH := macho64
endif

tests/%/code.s: tests/%/code.snek src/main.rs
	cargo run -- $< tests/$*/code.s

tests/%/code.run: tests/%/code.s runtime/start.rs
	nasm -f $(ARCH) tests/$*/code.s -o tests/$*/code.o
	ar rcs tests/$*/libcode.a tests/$*/code.o
	rustc -C debuginfo=2 -g -L tests/$*/ -lour_code:code runtime/start.rs -o tests/$*/code.run

cleanall:
	rm -rf tests/*/*.a tests/*/*.s tests/*/*.run tests/*/*.o
