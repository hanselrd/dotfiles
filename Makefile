ENV := $(PWD)/.env
ifneq (,$(wildcard $(ENV)))
	include $(ENV)
	export
endif

DHALLC = dhall
DHALL2YAMLC = dhall-to-yaml

SRCDIR := src
OBJDIR := build

SRCS := $(shell find $(SRCDIR) -type f -name "*.dhall" -not -name "*.partial.dhall" -not -name "*.template.dhall")
TEMPLATE_SRCS := $(shell find $(SRCDIR) -type f -name "*.template.dhall")
PARTIALS := $(shell find $(SRCDIR) -type f -name "*.partial.dhall")
OBJS := $(SRCS:$(SRCDIR)/%.dhall=$(OBJDIR)/%.yml)
TOBJS := $(TEMPLATE_SRCS:$(SRCDIR)/%.template.dhall=$(OBJDIR)/%)

.PHONY: all lint format freeze clean

all: $(OBJS) $(TOBJS)

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

$(OBJDIR):
	@mkdir -p $@

lint:
	@$(DHALLC) lint --check $(SRCS) $(TEMPLATE_SRCS) $(PARTIALS)
	@$(DHALLC) format --check $(SRCS) $(TEMPLATE_SRCS) $(PARTIALS)
	@$(DHALLC) freeze --check $(SRCS) $(TEMPLATE_SRCS) $(PARTIALS)

format:
	@$(DHALLC) lint $(SRCS) $(TEMPLATE_SRCS) $(PARTIALS)
	@$(DHALLC) format $(SRCS) $(TEMPLATE_SRCS) $(PARTIALS)

freeze:
	@$(DHALLC) freeze $(SRCS) $(TEMPLATE_SRCS) $(PARTIALS)

clean:
	@rm -rf $(OBJDIR)