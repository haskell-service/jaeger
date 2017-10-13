THRIFT = thrift

JAEGER_THRIFT = vendor/jaeger-idl/thrift
OUT_DIR = gen-hs

thrift:
	@mkdir -p $(OUT_DIR)
	$(THRIFT) --gen hs -r -out $(OUT_DIR) $(JAEGER_THRIFT)/agent.thrift

.PHONY: thrift
