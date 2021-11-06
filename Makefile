ENV := $(PWD)/.env
ifneq (,$(wildcard $(ENV)))
	include $(ENV)
	export
endif

DHALLC = dhall
DHALL2YAMLC = dhall-to-yaml

SRCDIR := src
OBJDIR := build

SRCS := $(shell find $(SRCDIR) -type f -name "*.dhall" -not -name "environment.dhall" -not -name "*.partial.dhall" -not -name "*.template.dhall")
TEMPLATE_SRCS := $(shell find $(SRCDIR) -type f -name "*.template.dhall")
PARTIAL_SRCS := $(shell find $(SRCDIR) -type f -name "*.partial.dhall")
RAW_SRCS := $(shell find $(SRCDIR) -type f -not -name "*.dhall")
OBJS := $(SRCS:$(SRCDIR)/%.dhall=$(OBJDIR)/%.yml)
TOBJS := $(TEMPLATE_SRCS:$(SRCDIR)/%.template.dhall=$(OBJDIR)/%)
ROBJS := $(RAW_SRCS:$(SRCDIR)/%=$(OBJDIR)/%)

.PHONY: all lint format freeze clean

all: $(OBJS) $(TOBJS) $(ROBJS)

$(OBJDIR)/environment.dhall: $(SRCDIR)/environment.dhall .env | $(OBJDIR)
	@echo "[Generating environment] $< --> $@"
	@$(DHALLC) text --file $< --output $@

$(OBJDIR)/%.yml: $(SRCDIR)/%.dhall $(OBJDIR)/environment.dhall | $(OBJDIR)
	@echo "[Generating source] $< --> $@"
	@mkdir -p $(@D)
	@$(DHALL2YAMLC) --generated-comment --file $< --output $@

$(OBJDIR)/%: $(SRCDIR)/%.template.dhall $(OBJDIR)/environment.dhall | $(OBJDIR)
	@echo "[Generating template] $< --> $@"
	@mkdir -p $(@D)
	@$(DHALLC) text --file $< --output $@

$(OBJDIR)/%: $(SRCDIR)/% | $(OBJDIR)
	@echo "[Copying file] $< --> $@"
	@mkdir -p $(@D)
	@cp $< $@

$(OBJDIR):
	@mkdir -p $@

lint:
	@$(DHALLC) lint --check $(SRCS) $(TEMPLATE_SRCS) $(PARTIAL_SRCS)
	@$(DHALLC) format --check $(SRCS) $(TEMPLATE_SRCS) $(PARTIAL_SRCS)
	@$(DHALLC) freeze --check $(SRCS) $(TEMPLATE_SRCS) $(PARTIAL_SRCS)

format:
	@$(DHALLC) lint $(SRCS) $(TEMPLATE_SRCS) $(PARTIAL_SRCS)
	@$(DHALLC) format $(SRCS) $(TEMPLATE_SRCS) $(PARTIAL_SRCS)

freeze:
	@$(DHALLC) freeze $(SRCS) $(TEMPLATE_SRCS) $(PARTIAL_SRCS)

clean:
	@rm -rf $(OBJDIR)