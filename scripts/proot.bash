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
# dir_exists(path: Text)
dir_exists__39_v0() {
    local path_11="${1}"
    [ -d "${path_11}" ]
    __status=$?
    ret_dir_exists39_v0="$(( __status == 0 ))"
    return 0
}

# file_exists(path: Text)
file_exists__40_v0() {
    local path_27="${1}"
    [ -f "${path_27}" ]
    __status=$?
    ret_file_exists40_v0="$(( __status == 0 ))"
    return 0
}

# dir_create(path: Text)
dir_create__45_v0() {
    local path_10="${1}"
    dir_exists__39_v0 "${path_10}"
    local ret_dir_exists39_v0__87_12="${ret_dir_exists39_v0}"
    if [ "$(( ! ret_dir_exists39_v0__87_12 ))" != 0 ]; then
        mkdir -p "${path_10}"
        __status=$?
        if [ "${__status}" != 0 ]; then
            ret_dir_create45_v0=''
            return "${__status}"
        fi
    fi
}

# file_chmod(path: Text, mode: Text)
file_chmod__48_v0() {
    local path_25="${1}"
    local mode_26="${2}"
    file_exists__40_v0 "${path_25}"
    local ret_file_exists40_v0__153_8="${ret_file_exists40_v0}"
    if [ "${ret_file_exists40_v0__153_8}" != 0 ]; then
        chmod "${mode_26}" "${path_25}"
        __status=$?
        if [ "${__status}" != 0 ]; then
            ret_file_chmod48_v0=''
            return "${__status}"
        fi
        ret_file_chmod48_v0=''
        return 0
    fi
    echo "The file ${path_25} doesn't exist"'!'""
    ret_file_chmod48_v0=''
    return 1
}

# env_var_get(name: Text)
env_var_get__121_v0() {
    local name_4="${1}"
    if [ "$([ "_${EXEC_SHELL}" != "_bash" ]; echo $?)" != 0 ]; then
        local command_0
        command_0="$(printf "%s
" "${!name_4}")"
        __status=$?
        if [ "${__status}" != 0 ]; then
            ret_env_var_get121_v0=''
            return "${__status}"
        fi
        ret_env_var_get121_v0="${command_0}"
        return 0
    elif [ "$([ "_${EXEC_SHELL}" != "_zsh" ]; echo $?)" != 0 ]; then
        local command_1
        command_1="$(printf "%s
" "${(P)name_4}")"
        __status=$?
        if [ "${__status}" != 0 ]; then
            ret_env_var_get121_v0=''
            return "${__status}"
        fi
        ret_env_var_get121_v0="${command_1}"
        return 0
    elif [ "$([ "_${EXEC_SHELL}" != "_ksh" ]; echo $?)" != 0 ]; then
        local command_2
        command_2="$(eval "echo \${$name_4}")"
        __status=$?
        if [ "${__status}" != 0 ]; then
            ret_env_var_get121_v0=''
            return "${__status}"
        fi
        ret_env_var_get121_v0="${command_2}"
        return 0
    fi
}

# is_command(command: Text)
is_command__125_v0() {
    local command_20="${1}"
    [ -x "$(command -v "${command_20}")" ]
    __status=$?
    if [ "${__status}" != 0 ]; then
        ret_is_command125_v0=0
        return 0
    fi
    ret_is_command125_v0=1
    return 0
}

# file_download(url: Text, path: Text)
file_download__241_v0() {
    local url_18="${1}"
    local path_19="${2}"
    is_command__125_v0 "curl"
    local ret_is_command125_v0__15_9="${ret_is_command125_v0}"
    is_command__125_v0 "wget"
    local ret_is_command125_v0__18_9="${ret_is_command125_v0}"
    is_command__125_v0 "aria2c"
    local ret_is_command125_v0__21_9="${ret_is_command125_v0}"
    if [ "${ret_is_command125_v0__15_9}" != 0 ]; then
        curl -L -o "${path_19}" "${url_18}" >/dev/null 2>&1
        __status=$?
    elif [ "${ret_is_command125_v0__18_9}" != 0 ]; then
        wget "${url_18}" -P "${path_19}" >/dev/null 2>&1
        __status=$?
    elif [ "${ret_is_command125_v0__21_9}" != 0 ]; then
        aria2c "${url_18}" -d "${path_19}" >/dev/null 2>&1
        __status=$?
    else
        ret_file_download241_v0=''
        return 1
    fi
}

env_var_get__121_v0 "HOME"
__status=$?
if [ "${__status}" != 0 ]; then
    exit "${__status}"
fi
ret_env_var_get121_v0__6_27="${ret_env_var_get121_v0}"
install_dir_5="${ret_env_var_get121_v0__6_27}/.bootstrap"
proot_bin_6="${install_dir_5}/proot"
dir_create__45_v0 "${install_dir_5}"
__status=$?
if [ "${__status}" != 0 ]; then
    exit "${__status}"
fi
file_download__241_v0 "https://proot.gitlab.io/proot/bin/proot" "${proot_bin_6}"
__status=$?
if [ "${__status}" != 0 ]; then
    exit "${__status}"
fi
file_chmod__48_v0 "${proot_bin_6}" "755"
__status=$?
if [ "${__status}" != 0 ]; then
    exit "${__status}"
fi
