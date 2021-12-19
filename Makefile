ENV := $(PWD)/.env
ifneq (,$(wildcard $(ENV)))
	include $(ENV)
	export
endif

DHALLC = dhall
DHALL2YAMLC = dhall-to-yaml

SRCDIR := src
OBJDIR := build
PACKAGEDIR := packages
REPORTDIR := reports

SRCS := $(shell find $(SRCDIR) -type f \
			-name "*.dhall" \
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

.PHONY: all lint format freeze install package report test clean

all: $(OBJS) \
	$(TREE_OBJS) \
	$(TEMPLATE_OBJS) \
	$(RAW_OBJS)

$(OBJDIR)/%.yml: $(SRCDIR)/%.dhall $(CODEGEN_TREE_OBJS) $(CODEGEN_TEMPLATE_OBJS) | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building source]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$@$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALL2YAMLC) --generated-comment --file $< --output $@

$(SRCDIR)/codegen/%: $(SRCDIR)/%.ctree.dhall | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building codegen directory tree]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$(@D)$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALLC) to-directory-tree --file $< --output $(@D)

$(SRCDIR)/codegen/%: $(SRCDIR)/%.ctemplate.dhall | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building codegen template]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$@$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALLC) text --file $< --output $@

$(OBJDIR)/%: $(SRCDIR)/%.tree.dhall $(CODEGEN_TREE_OBJS) $(CODEGEN_TEMPLATE_OBJS) | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building directory tree]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$(@D)$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALLC) to-directory-tree --file $< --output $(@D)

$(OBJDIR)/%: $(SRCDIR)/%.template.dhall $(CODEGEN_TREE_OBJS) $(CODEGEN_TEMPLATE_OBJS) | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Building template]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$@$(CHALK_RESET)"
	@mkdir -p $(@D)
	@$(DHALLC) text --file $< --output $@

$(OBJDIR)/%: $(SRCDIR)/% | $(OBJDIR)
	@echo "$(CHALK_WHITE)[Copying file]$(CHALK_RESET) $(CHALK_YELLOW)$<$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$@$(CHALK_RESET)"
	@mkdir -p $(@D)
	@cp $< $@

$(OBJDIR) $(PACKAGEDIR) $(REPORTDIR):
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

package: all | $(PACKAGEDIR)
	@set -e ;\
	PACKAGE=$(PACKAGEDIR)/package-$(DOTFILES_CONFIGURATION)-$(DOTFILES_PACKAGE_MANAGER)-$$(date --utc +%Y%m%dT%H%M%SZ).tar.gz ;\
	echo "$(CHALK_WHITE)[Building package]$(CHALK_RESET) $(CHALK_YELLOW)$(OBJDIR)$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$$PACKAGE$(CHALK_RESET)" ;\
	tar --transform='s/$(OBJDIR)/package/g' -czvf $$PACKAGE $(OBJDIR)

report: $(CODEGEN_TREE_OBJS) $(CODEGEN_TEMPLATE_OBJS) | $(REPORTDIR)
	@set -e ;\
	REPORT=$(REPORTDIR)/report-$$(date --utc +%Y%m%dT%H%M%SZ).txt ;\
	echo "$(CHALK_WHITE)[Generating report]$(CHALK_RESET) $(CHALK_YELLOW)REPORT$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$$REPORT$(CHALK_RESET)" ;\
    truncate -s 0 $$REPORT ;\
    find $(SRCDIR) -type f -name "*.dhall" -exec sh -c "printf \"%s \" {} >> $$REPORT" \; -exec sh -c "$(DHALLC) --file {} | $(DHALLC) encode | wc -c >> $$REPORT" \; ;\
	echo "$(CHALK_WHITE)[Sorting report]$(CHALK_RESET) $(CHALK_YELLOW)$$REPORT$(CHALK_RESET) $(CHALK_WHITE)-->$(CHALK_RESET) $(CHALK_GREEN)$$REPORT.sorted$(CHALK_RESET)" ;\
	cat $$REPORT | sort -t" " -rnk2 > $$REPORT.sorted

test:
	@env --chdir=$(OBJDIR) ansible-playbook --check --diff -i inventory playbook.yml

clean:
	@rm -rf $(SRCDIR)/codegen $(OBJDIR) $(PACKAGEDIR) $(REPORTDIR)
