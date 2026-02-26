SHELL := /bin/sh
COBC := cobc
COBFLAGS := -x -free
SRC_DIR := src
COPY_DIR := copybooks
BIN_DIR := bin

SOURCES := $(wildcard $(SRC_DIR)/*.cob)
PROGRAMS := $(patsubst $(SRC_DIR)/%.cob,$(BIN_DIR)/%,$(SOURCES))

DATA_DIR := data

.PHONY: all clean test data

all: $(PROGRAMS)

$(BIN_DIR)/%: $(SRC_DIR)/%.cob | $(BIN_DIR)
	$(COBC) $(COBFLAGS) -I $(COPY_DIR) -o $@ $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

data:
	mkdir -p $(DATA_DIR)

test: data
	./run-tests.sh

clean:
	rm -f $(BIN_DIR)/*
	rm -f $(DATA_DIR)/*.dat
