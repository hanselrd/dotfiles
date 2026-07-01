#!/usr/bin/env zsh
# Written in [Amber](https://amber-lang.com/)
# version: 0.6.0-alpha-nix-4a8f161
emulate ksh
setopt BSD_echo
__read_args='-A'
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
# trim(text: Text)
trim__10_v0() {
    local text_14="${1}"
    local result_15=""
    result_15="${text_14#${text_14%%[![:space:]]*}}"
    result_15="${result_15%${result_15##*[![:space:]]}}"
    __status=$?
    ret_trim10_v0="${result_15}"
    return 0
}

# file_exists(path: Text)
file_exists__40_v0() {
    local path_48="${1}"
    [ -f "${path_48}" ]
    __status=$?
    ret_file_exists40_v0="$(( __status == 0 ))"
    return 0
}

# is_mac_os_mktemp()
is_mac_os_mktemp__46_v0() {
    mktemp --version >/dev/null 2>&1
    __status=$?
    if [ "${__status}" != 0 ]; then
        ret_is_mac_os_mktemp46_v0=1
        return 0
    fi
    ret_is_mac_os_mktemp46_v0=0
    return 0
}

# temp_dir_create(template: Text, auto_delete: Bool, force_delete: Bool)
temp_dir_create__47_v0() {
    local template_11="${1}"
    local auto_delete_12="${2}"
    local force_delete_13="${3}"
    trim__10_v0 "${template_11}"
    local ret_trim10_v0__113_8="${ret_trim10_v0}"
    if [ "$([ "_${ret_trim10_v0__113_8}" != "_" ]; echo $?)" != 0 ]; then
        echo "The template cannot be an empty string"'!'""
        ret_temp_dir_create47_v0=''
        return 1
    fi
    local filename_16=""
    is_mac_os_mktemp__46_v0 
    local ret_is_mac_os_mktemp46_v0__119_8="${ret_is_mac_os_mktemp46_v0}"
    if [ "${ret_is_mac_os_mktemp46_v0__119_8}" != 0 ]; then
        local command_0
        command_0="$(mktemp -d -p "$TMPDIR" "${template_11}")"
        __status=$?
        if [ "${__status}" != 0 ]; then
            ret_temp_dir_create47_v0=''
            return "${__status}"
        fi
        filename_16="${command_0}"
    else
        local command_1
        command_1="$(mktemp -d -p "$TMPDIR" -t "${template_11}")"
        __status=$?
        if [ "${__status}" != 0 ]; then
            ret_temp_dir_create47_v0=''
            return "${__status}"
        fi
        filename_16="${command_1}"
    fi
    if [ "$([ "_${filename_16}" != "_" ]; echo $?)" != 0 ]; then
        echo "Failed to make a temporary directory"
        ret_temp_dir_create47_v0=''
        return 1
    fi
    if [ "$(( auto_delete_12 && $([ "_${EXEC_SHELL}" == "_ksh" ]; echo $?) ))" != 0 ]; then
        if [ "${force_delete_13}" != 0 ]; then
            trap 'rm -rf '"${filename_16}"'' EXIT
            __status=$?
            if [ "${__status}" != 0 ]; then
                echo "Setting auto deletion fails. You must delete temporary dir ${filename_16}."
            fi
        else
            trap 'rmdir '"${filename_16}"'' EXIT
            __status=$?
            if [ "${__status}" != 0 ]; then
                echo "Setting auto deletion fails. You must delete temporary dir ${filename_16}."
            fi
        fi
    fi
    ret_temp_dir_create47_v0="${filename_16}"
    return 0
}

# file_chmod(path: Text, mode: Text)
file_chmod__48_v0() {
    local path_46="${1}"
    local mode_47="${2}"
    file_exists__40_v0 "${path_46}"
    local ret_file_exists40_v0__153_8="${ret_file_exists40_v0}"
    if [ "${ret_file_exists40_v0__153_8}" != 0 ]; then
        chmod "${mode_47}" "${path_46}"
        __status=$?
        if [ "${__status}" != 0 ]; then
            ret_file_chmod48_v0=''
            return "${__status}"
        fi
        ret_file_chmod48_v0=''
        return 0
    fi
    echo "The file ${path_46} doesn't exist"'!'""
    ret_file_chmod48_v0=''
    return 1
}

# is_command(command: Text)
is_command__125_v0() {
    local command_41="${1}"
    [ -x "$(command -v "${command_41}")" ]
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
    local url_39="${1}"
    local path_40="${2}"
    is_command__125_v0 "curl"
    local ret_is_command125_v0__15_9="${ret_is_command125_v0}"
    is_command__125_v0 "wget"
    local ret_is_command125_v0__18_9="${ret_is_command125_v0}"
    is_command__125_v0 "aria2c"
    local ret_is_command125_v0__21_9="${ret_is_command125_v0}"
    if [ "${ret_is_command125_v0__15_9}" != 0 ]; then
        curl -L -o "${path_40}" "${url_39}" >/dev/null 2>&1
        __status=$?
    elif [ "${ret_is_command125_v0__18_9}" != 0 ]; then
        wget "${url_39}" -P "${path_40}" >/dev/null 2>&1
        __status=$?
    elif [ "${ret_is_command125_v0__21_9}" != 0 ]; then
        aria2c "${url_39}" -d "${path_40}" >/dev/null 2>&1
        __status=$?
    else
        ret_file_download241_v0=''
        return 1
    fi
}

# nix_install_download(version: Null, path: Text)
nix_install_download__244_v0() {
    local version_37="${1}"
    local path_38="${2}"
        file_download__241_v0 "https://nixos.org/nix/install" "${path_38}"
        __status=$?
        if [ "${__status}" != 0 ]; then
            ret_nix_install_download244_v0=''
            return "${__status}"
        fi
        ret_nix_install_download244_v0=''
        return 0
}

temp_dir_create__47_v0 "nix.XXXXXX" 1 1
__status=$?
if [ "${__status}" != 0 ]; then
    exit "${__status}"
fi
tmp_dir_17="${ret_temp_dir_create47_v0}"
install_sh_18="${tmp_dir_17}/install.sh"
nix_install_download__244_v0 '' "${install_sh_18}"
__status=$?
if [ "${__status}" != 0 ]; then
    exit "${__status}"
fi
file_chmod__48_v0 "${install_sh_18}" "755"
__status=$?
if [ "${__status}" != 0 ]; then
    exit "${__status}"
fi
${install_sh_18} --no-daemon
__status=$?
if [ "${__status}" != 0 ]; then
    echo "Failed to run install script"
fi
