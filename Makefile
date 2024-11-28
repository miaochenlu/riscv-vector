base_dir=$(abspath ./)

CHISEL_VERSION=3.6.0
MODEL ?= TestHarness
# PROJECT ?= freechips.rocketchip.system
PROJECT ?= gpc.system
CFG_PROJECT ?= $(PROJECT)
CONFIG ?= $(CFG_PROJECT).DefaultConfig
MILL ?= mill

verilog:
	cd dependencies/gpc_cache && git checkout gpc_cache_stable && git pull && cd -
	rm -rf src/main/scala/gpcdcache && mkdir src/main/scala/gpcdcache/
	cp dependencies/gpc_cache/src/main/scala/grapecoveDcache/* src/main/scala/gpcdcache/
	cd $(base_dir) && $(MILL) emulator[gpc.system.TestHarness,$(CONFIG)].mfccompiler.compile

clean:
	rm -rf src/main/scala/gpcdcache && rm -rf out/
