#!/usr/bin/env bash
# Written in [Amber](https://amber-lang.com/)
# version: 0.6.0-alpha-nix-4a8f161
[ "$EUID" -ne 0 ] && { { command -v sudo >/dev/null 2>&1 && __sudo=sudo; } || { command -v doas >/dev/null 2>&1 && __sudo=doas; }; }
if [ -n "$ZSH_VERSION" ]; then
    EXEC_SHELL="zsh"
    IFS='.' read -r -A EXEC_SHELL_VERSION <<< "$ZSH_VERSION"
elif [ -n "$KSH_VERSION" ]; then
    EXEC_SHELL="ksh"
    __exec_shell_version="${.sh.version##*/}"
    IFS='.' read -r -a EXEC_SHELL_VERSION <<< "${__exec_shell_version%% *}"
else
    EXEC_SHELL="bash"
    EXEC_SHELL_VERSION=("${BASH_VERSINFO[0]}" "${BASH_VERSINFO[1]}" "${BASH_VERSINFO[2]}")
fi
# join(list: [Text], delimiter: Text)
join__7_v0() {
    local list_11=("${!1}")
    local delimiter_12="${2}"
    local command_0
    command_0="$(IFS="${delimiter_12}" ; printf "%s
" "${list_11[*]}")"
    __status=$?
    ret_join7_v0="${command_0}"
    return 0
}

# env_var_set(name: Text, val: Text)
env_var_set__120_v0() {
    local name_16="${1}"
    local val_17="${2}"
    export $name_16="$val_17" 2> /dev/null
    __status=$?
    if [ "${__status}" != 0 ]; then
        ret_env_var_set120_v0=''
        return "${__status}"
    fi
}

# nix_config_option(name: Text, values: [Text])
nix_config_option__162_v0() {
    local name_9="${1}"
    local values_10=("${!2}")
    join__7_v0 values_10[@] " "
    local ret_join7_v0__5_23="${ret_join7_v0}"
    ret_nix_config_option162_v0="${name_9} = ${ret_join7_v0__5_23}"
    return 0
}

array_3=("nix-command" "flakes" "pipe-operators")
nix_config_option__162_v0 "experimental-features" array_3[@]
ret_nix_config_option162_v0__10_9="${ret_nix_config_option162_v0}"
array_4=("true")
nix_config_option__162_v0 "show-trace" array_4[@]
ret_nix_config_option162_v0__11_9="${ret_nix_config_option162_v0}"
nix_config_13=("${ret_nix_config_option162_v0__10_9}" "${ret_nix_config_option162_v0__11_9}")
join__7_v0 nix_config_13[@] "
"
ret_join7_v0__14_31="${ret_join7_v0}"
env_var_set__120_v0 "NIX_CONFIG" "${ret_join7_v0__14_31}"
__status=$?
if [ "${__status}" != 0 ]; then
    exit "${__status}"
fi
