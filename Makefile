
BUILD_DIR = ./build

PRJ = gpcFU

init:
	git submodule update --init
	cd dependencies/rocket-chip && git submodule update --init dependencies/hardfloat dependencies/cde dependencies/diplomacy

test:
	mill -i $(PRJ).test

verilog:
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ).runMain $(PRJ).Main --target-dir $(BUILD_DIR)

help:
	mill -i $(PRJ).runMain Gcd.DCache --help

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

clean:
	-rm -rf $(BUILD_DIR)
	rm -rf ./out
	rm -rf ./test_run_dir

include Makefile.test

.PHONY: test verilog help reformat checkformat clean