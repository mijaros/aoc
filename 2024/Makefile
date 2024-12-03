GO=/usr/bin/go
MY_BINS=$(wildcard cmd/*)
MY_TARGETS=$(patsubst cmd/%,%,$(MY_BINS))
SOURCES=$(patsubst %,%/main.go,$(MY_BINS))
TARGETS=$(patsubst cmd/%,bin/%,$(MY_BINS))
TEST_DIRS=$(patsubst cmd/%,inputs/%/,$(MY_BINS))
TEST_FILES=$(shell find inputs -type f -print)

all: $(TARGETS)

.NOTPARALLEL:
.SECONDEXPANSION:

$(MY_TARGETS):
DAY_TARGET=$(wildcard inputs/$@/*)
$(MY_TARGETS): %: bin/% $$(DAY_TARGET)


$(TEST_FILES): $$(patsubst inputs/%/,bin/%,$$(dir $$@))
	@echo EXECUTING $< with $@
	$< -iFile $@

$(TEST_DIRS): inputs/%: bin/%

$(TARGETS): bin/%: cmd/%/main.go
	go build -o $@ $<



clean:
	rm -f bin/*


setup_day:
ifndef DAY_INI
	$(error VARIABLE DAY_INI not set)
endif
	mkdir -p {cmd,inputs}/$(DAY_INI)
	cp ./main.go.tpl cmd/$(DAY_INI)/main.go


.PHONY: $(TEST_FILES) all $(MY_TARGETS)
