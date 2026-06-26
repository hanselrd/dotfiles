#!/usr/bin/env bash
# Written in [Amber](https://amber-lang.com/)
# version: 0.5.1-alpha
trim_left__9_v0() {
  local text=$1
  command_0="$(echo "${text}" | sed -e 's/^[[:space:]]*//')"
  __status=$?
  ret_trim_left9_v0="${command_0}"
  return 0
}

trim_right__10_v0() {
  local text=$1
  command_1="$(echo "${text}" | sed -e 's/[[:space:]]*$//')"
  __status=$?
  ret_trim_right10_v0="${command_1}"
  return 0
}

trim__11_v0() {
  local text=$1
  trim_right__10_v0 "${text}"
  ret_trim_right10_v0__178_22="${ret_trim_right10_v0}"
  trim_left__9_v0 "${ret_trim_right10_v0__178_22}"
  ret_trim11_v0="${ret_trim_left9_v0}"
  return 0
}

file_exists__37_v0() {
  local path=$1
  [ -f "${path}" ]
  __status=$?
  ret_file_exists37_v0="$((__status == 0))"
  return 0
}

is_mac_os_mktemp__43_v0() {
  mktemp --version > /dev/null 2>&1
  __status=$?
  if [ "${__status}" != 0 ]; then
    ret_is_mac_os_mktemp43_v0=1
    return 0
  fi
  ret_is_mac_os_mktemp43_v0=0
  return 0
}

temp_dir_create__44_v0() {
  local template=$1
  local auto_delete=$2
  local force_delete=$3
  trim__11_v0 "${template}"
  ret_trim11_v0__113_8="${ret_trim11_v0}"
  if [ "$(
    [ "_${ret_trim11_v0__113_8}" != "_" ]
    echo $?
  )" != 0 ]; then
    echo "The template cannot be an empty string"'!'""
    ret_temp_dir_create44_v0=''
    return 1
  fi
  filename_3=""
  is_mac_os_mktemp__43_v0
  ret_is_mac_os_mktemp43_v0__119_8="${ret_is_mac_os_mktemp43_v0}"
  if [ "${ret_is_mac_os_mktemp43_v0__119_8}" != 0 ]; then
    command_2="$(mktemp -d -p "$TMPDIR" "${template}")"
    __status=$?
    if [ "${__status}" != 0 ]; then
      ret_temp_dir_create44_v0=''
      return "${__status}"
    fi
    filename_3="${command_2}"
  else
    command_3="$(mktemp -d -p "$TMPDIR" -t "${template}")"
    __status=$?
    if [ "${__status}" != 0 ]; then
      ret_temp_dir_create44_v0=''
      return "${__status}"
    fi
    filename_3="${command_3}"
  fi
  if [ "$(
    [ "_${filename_3}" != "_" ]
    echo $?
  )" != 0 ]; then
    echo "Failed to make a temporary directory"
    ret_temp_dir_create44_v0=''
    return 1
  fi
  if [ "${auto_delete}" != 0 ]; then
    if [ "${force_delete}" != 0 ]; then
      trap 'rm -rf '"${filename_3}"'' EXIT
      __status=$?
      if [ "${__status}" != 0 ]; then
        echo "Setting auto deletion fails. You must delete temporary dir ${filename_3}."
      fi
    else
      trap 'rmdir '"${filename_3}"'' EXIT
      __status=$?
      if [ "${__status}" != 0 ]; then
        echo "Setting auto deletion fails. You must delete temporary dir ${filename_3}."
      fi
    fi
  fi
  ret_temp_dir_create44_v0="${filename_3}"
  return 0
}

file_chmod__45_v0() {
  local path=$1
  local mode=$2
  file_exists__37_v0 "${path}"
  ret_file_exists37_v0__153_8="${ret_file_exists37_v0}"
  if [ "${ret_file_exists37_v0__153_8}" != 0 ]; then
    chmod "${mode}" "${path}"
    __status=$?
    if [ "${__status}" != 0 ]; then
      ret_file_chmod45_v0=''
      return "${__status}"
    fi
    ret_file_chmod45_v0=''
    return 0
  fi
  echo "The file ${path} doesn't exist"'!'""
  ret_file_chmod45_v0=''
  return 1
}

is_command__100_v0() {
  local command=$1
  [ -x "$(command -v "${command}")" ]
  __status=$?
  if [ "${__status}" != 0 ]; then
    ret_is_command100_v0=0
    return 0
  fi
  ret_is_command100_v0=1
  return 0
}

file_download__146_v0() {
  local url=$1
  local path=$2
  is_command__100_v0 "curl"
  ret_is_command100_v0__14_9="${ret_is_command100_v0}"
  is_command__100_v0 "wget"
  ret_is_command100_v0__17_9="${ret_is_command100_v0}"
  is_command__100_v0 "aria2c"
  ret_is_command100_v0__20_9="${ret_is_command100_v0}"
  if [ "${ret_is_command100_v0__14_9}" != 0 ]; then
    curl -L -o "${path}" "${url}" > /dev/null 2>&1
    __status=$?
  elif [ "${ret_is_command100_v0__17_9}" != 0 ]; then
    wget "${url}" -P "${path}" > /dev/null 2>&1
    __status=$?
  elif [ "${ret_is_command100_v0__20_9}" != 0 ]; then
    aria2c "${url}" -d "${path}" > /dev/null 2>&1
    __status=$?
  else
    ret_file_download146_v0=''
    return 1
  fi
}

nix_install_download__148_v0() {
  local version=$1
  local path=$2
  if [ 0 != 0 ]; then
    file_download__146_v0 "https://releases.nixos.org/nix/nix-${version}/install" "${path}"
    __status=$?
    if [ "${__status}" != 0 ]; then
      ret_nix_install_download148_v0=''
      return "${__status}"
    fi
    ret_nix_install_download148_v0=''
    return 0
  elif [ 1 != 0 ]; then
    file_download__146_v0 "https://nixos.org/nix/install" "${path}"
    __status=$?
    if [ "${__status}" != 0 ]; then
      ret_nix_install_download148_v0=''
      return "${__status}"
    fi
    ret_nix_install_download148_v0=''
    return 0
  fi
}

temp_dir_create__44_v0 "nix.XXXXXX" 1 1
__status=$?
if [ "${__status}" != 0 ]; then
  exit "${__status}"
fi
tmp_dir_4="${ret_temp_dir_create44_v0}"
install_sh_5="${tmp_dir_4}/install.sh"
nix_install_download__148_v0 '' "${install_sh_5}"
__status=$?
if [ "${__status}" != 0 ]; then
  exit "${__status}"
fi
file_chmod__45_v0 "${install_sh_5}" "755"
__status=$?
if [ "${__status}" != 0 ]; then
  exit "${__status}"
fi
${install_sh_5} --no-daemon
__status=$?
if [ "${__status}" != 0 ]; then
  echo "Failed to run install script"
fi
