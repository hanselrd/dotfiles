let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  ''
    [user]
    	email = ${env.user_email}
    	name = ${env.user_name}
    [color]
    	ui = auto
    [color "branch"]
    	current = yellow reverse
    	local = yellow
    	remote = green
    [color "diff"]
    	meta = yellow bold
    	frag = magenta bold
    	old = red bold
    	new = green bold
    [color "status"]
    	added = yellow
    	changed = green
    	untracked = cyan
    [core]
    	excludesfile=${Directory/toText Directory.Git}/.gitignore
    	editor = $(which vim)
    [grep]
    	lineNumber = true
    [merge]
    	conflictStyle = diff3
    [alias]
    	lgb = log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset%n' --abbrev-commit --date=relative --branches
    	l = log --graph --oneline --decorate
    	ll = log --graph --oneline --decorate --branches --tags
    	lll = log --graph --oneline --decorate --all
    ''
