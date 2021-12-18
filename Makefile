ENV := $(PWD)/.env
ifneq (,$(wildcard $(ENV)))
	include $(ENV)
	export
endif

DHALLC = dhall
DHALL2YAMLC = dhall-to-yaml

SRCDIR := src
OBJDIR := build

SRCS := $(shell find $(SRCDIR) -type f \
			-name "*.dhall" \
			-not -name "environment.dhall" \
			-not -name "*.ctree.dhall" \
			-not -name "*.ctemplate.dhall" \
			-not -name "*.tree.dhall" \
			-not -name "*.template.dhall" \
			-not -name "*.partial.dhall")
CODEGEN_TREE_SRCS := $(shell find $(SRCDIR) -type f -name "*.ctree.dhall")
CODEGEN_TEMPLATE_SRCS := $(shell find $(SRCDIR) -type f -name "*.ctemplate.dhall")
TREE_SRCS := $(shell find $(SRCDIR) -type f -name "*.tree.dhall")
TEMPLATE_SRCS := $(shell find $(SRCDIR) -type f -name "*.template.dhall")
PARTIAL_SRCS := $(shell find $(SRCDIR) -type f -name "*.partial.dhall")
RAW_SRCS := $(shell find $(SRCDIR) -type f -not -name "*.dhall")
OBJS := $(SRCS:$(SRCDIR)/%.dhall=$(OBJDIR)/%.yml)
CODEGEN_TREE_OBJS := $(CODEGEN_TREE_SRCS:$(SRCDIR)/%.ctree.dhall=$(SRCDIR)/codegen/%)
CODEGEN_TEMPLATE_OBJS := $(CODEGEN_TEMPLATE_SRCS:$(SRCDIR)/%.ctemplate.dhall=$(SRCDIR)/codegen/%)
TREE_OBJS := $(TREE_SRCS:$(SRCDIR)/%.tree.dhall=$(OBJDIR)/%)
TEMPLATE_OBJS := $(TEMPLATE_SRCS:$(SRCDIR)/%.template.dhall=$(OBJDIR)/%)
RAW_OBJS := $(RAW_SRCS:$(SRCDIR)/%=$(OBJDIR)/%)

CHALK_RESET := $(shell tput sgr0)
CHALK_BLACK := $(shell tput setaf 0)
CHALK_RED := $(shell tput setaf 1)
CHALK_GREEN := $(shell tput setaf 2)
CHALK_YELLOW := $(shell tput setaf 3)
CHALK_BLUE := $(shell tput setaf 4)
CHALK_MAGENTA := $(shell tput setaf 5)
CHALK_CYAN := $(shell tput setaf 6)
CHALK_WHITE := $(shell tput setaf 7)

.PHONY: all lint format freeze install report test clean

all: $(OBJS) \
	$(TREE_OBJS) \
	$(TEMPLATE_OBJS) \
	$(RAW_OBJS)

$(OBJDIR)/environment.dhall: $(SRCDIR)/environment.dhall .env | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building environment]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$@$(CHALK_RESET)"
	@$(DHALLC) text --file $< --output $@

$(OBJDIR)/%.yml: $(SRCDIR)/%.dhall $(OBJDIR)/environment.dhall $(CODEGEN_TREE_OBJS) $(CODEGEN_TEMPLATE_OBJS) | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building source]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$@$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALL2YAMLC) --generated-comment --file $< --output $@

$(SRCDIR)/codegen/%: $(SRCDIR)/%.ctree.dhall $(OBJDIR)/environment.dhall | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building codegen directory tree]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$(@D)$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALLC) to-directory-tree --file $< --output $(@D)

$(SRCDIR)/codegen/%: $(SRCDIR)/%.ctemplate.dhall $(OBJDIR)/environment.dhall | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building codegen template]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$@$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALLC) text --file $< --output $@

$(OBJDIR)/%: $(SRCDIR)/%.tree.dhall $(OBJDIR)/environment.dhall $(CODEGEN_TREE_OBJS) $(CODEGEN_TEMPLATE_OBJS) | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building directory tree]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$(@D)$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALLC) to-directory-tree --file $< --output $(@D)

$(OBJDIR)/%: $(SRCDIR)/%.template.dhall $(OBJDIR)/environment.dhall $(CODEGEN_TREE_OBJS) $(CODEGEN_TEMPLATE_OBJS) | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building template]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$@$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALLC) text --file $< --output $@

$(OBJDIR)/%: $(SRCDIR)/% | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Copying file]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$@$(CHALK_RESET)"
	@mkdir -p $(@D)
	@cp $< $@

$(OBJDIR):
	@mkdir -p $@

lint:
	@$(DHALLC) lint --check \
		$(SRCS) \
		$(CODEGEN_TREE_SRCS) \
	    $(CODEGEN_TEMPLATE_SRCS) \
		$(TREE_SRCS) \
		$(TEMPLATE_SRCS) \
		$(PARTIAL_SRCS)
	@$(DHALLC) format --check \
		$(SRCS) \
		$(CODEGEN_TREE_SRCS) \
	    $(CODEGEN_TEMPLATE_SRCS) \
		$(TREE_SRCS) \
		$(TEMPLATE_SRCS) \
		$(PARTIAL_SRCS)
	@$(DHALLC) freeze --check \
		$(SRCS) \
		$(CODEGEN_TREE_SRCS) \
	    $(CODEGEN_TEMPLATE_SRCS) \
		$(TREE_SRCS) \
		$(TEMPLATE_SRCS) \
		$(PARTIAL_SRCS)

format:
	@$(DHALLC) lint \
		$(SRCS) \
		$(CODEGEN_TREE_SRCS) \
	    $(CODEGEN_TEMPLATE_SRCS) \
		$(TREE_SRCS) \
		$(TEMPLATE_SRCS) \
		$(PARTIAL_SRCS)
	@$(DHALLC) format \
		$(SRCS) \
		$(CODEGEN_TREE_SRCS) \
	    $(CODEGEN_TEMPLATE_SRCS) \
		$(TREE_SRCS) \
		$(TEMPLATE_SRCS) \
		$(PARTIAL_SRCS)

freeze:
	@$(DHALLC) freeze \
		$(SRCS) \
		$(CODEGEN_TREE_SRCS) \
	    $(CODEGEN_TEMPLATE_SRCS) \
		$(TREE_SRCS) \
		$(TEMPLATE_SRCS) \
		$(PARTIAL_SRCS)

install:
	@env --chdir=$(OBJDIR) ansible-playbook --diff -i inventory playbook.yml

report: $(CODEGEN_TREE_OBJS) $(CODEGEN_TEMPLATE_OBJS)
	@truncate -s 0 report.txt
	@$(foreach src,$(shell find $(SRCDIR) -type f -name "*.dhall"),printf "%s " $(src) >> report.txt;$(DHALLC) --file $(src) | $(DHALLC) encode | wc -c >> report.txt;)
	@cat report.txt | sort -t" " -nk2 -o report.txt

test:
	@env --chdir=$(OBJDIR) ansible-playbook --check --diff -i inventory playbook.yml

clean:
	@rm -rf $(SRCDIR)/codegen $(OBJDIR) report.txt
