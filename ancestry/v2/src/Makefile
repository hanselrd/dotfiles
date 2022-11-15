.PHONY: install test

install:
	@ansible-playbook --diff -i inventory playbook.yml

test:
	@ansible-playbook --check --diff -i inventory playbook.yml
