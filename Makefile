BUILD_DIR = ./build

TRIGGER_HOME = $(shell pwd)

default: sim-verilog

sim-verilog:
	mkdir -p $(BUILD_DIR)
	mill -i Trigger.runMain trigger.TopMain -td $(BUILD_DIR)

emu: sim-verilog
	cd $(TRIGGER_HOME)/difftest && $(MAKE) WITH_DRAMSIM3=1 EMU_TRACE=1 emu -j