DEFINITION_NAME := tiny-spec

include $(CURDIR)/../../../include.mk

# TODO: These files should specify just the names of different test-sets and let `include.mk` do the rest.

kscript: kscripts/1.kscript

golden: kscripts/1.kscript_output.golden

clean:
	rm -rf $(KOMPILED) kscripts/*.output

.PHONY: test golden clean %.test %.krun
