#!/usr/bin/env bash
# Written in [Amber](https://amber-lang.com/)
# version: 0.5.1-alpha
dir_exists__36_v0() {
  local path=$1
  [ -d "${path}" ]
  __status=$?
  ret_dir_exists36_v0="$((__status == 0))"
  return 0
}

file_exists__37_v0() {
  local path=$1
  [ -f "${path}" ]
  __status=$?
  ret_file_exists37_v0="$((__status == 0))"
  return 0
}

dir_create__42_v0() {
  local path=$1
  dir_exists__36_v0 "${path}"
  ret_dir_exists36_v0__87_12="${ret_dir_exists36_v0}"
  if [ "$((!${ret_dir_exists36_v0__87_12}))" != 0 ]; then
    mkdir -p "${path}"
    __status=$?
    if [ "${__status}" != 0 ]; then
      ret_dir_create42_v0=''
      return "${__status}"
    fi
  fi
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

env_var_get__98_v0() {
  local name=$1
  command_0="$(echo ${!name})"
  __status=$?
  if [ "${__status}" != 0 ]; then
    ret_env_var_get98_v0=''
    return "${__status}"
  fi
  ret_env_var_get98_v0="${command_0}"
  return 0
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

nix_user_chroot_download__148_v0() {
  local version=$1
  local arch=$2
  local path=$3
  file_download__146_v0 "https://github.com/nix-community/nix-user-chroot/releases/download/${version}/nix-user-chroot-bin-${version}-${arch}-unknown-linux-musl" "${path}"
  __status=$?
  if [ "${__status}" != 0 ]; then
    ret_nix_user_chroot_download148_v0=''
    return "${__status}"
  fi
  ret_nix_user_chroot_download148_v0=''
  return 0
}

env_var_get__98_v0 "HOME"
__status=$?
if [ "${__status}" != 0 ]; then
  exit "${__status}"
fi
ret_env_var_get98_v0__10_27="${ret_env_var_get98_v0}"
install_dir_3="${ret_env_var_get98_v0__10_27}/.bootstrap"
nix_user_chroot_bin_4="${install_dir_3}/nix-user-chroot"
dir_create__42_v0 "${install_dir_3}"
__status=$?
if [ "${__status}" != 0 ]; then
  exit "${__status}"
fi
nix_user_chroot_download__148_v0 "1.2.2" "x86_64" "${nix_user_chroot_bin_4}"
__status=$?
if [ "${__status}" != 0 ]; then
  exit "${__status}"
fi
file_chmod__45_v0 "${nix_user_chroot_bin_4}" "755"
__status=$?
if [ "${__status}" != 0 ]; then
  exit "${__status}"
fi
