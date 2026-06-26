#!/usr/bin/env bash
# Written in [Amber](https://amber-lang.com/)
# version: 0.5.1-alpha
join__8_v0() {
  local list=("${!1}")
  local delimiter=$2
  command_0="$(
    IFS="${delimiter}"
    echo "${list[*]}"
  )"
  __status=$?
  ret_join8_v0="${command_0}"
  return 0
}

env_var_set__97_v0() {
  local name=$1
  local val=$2
  export $name="$val" 2> /dev/null
  __status=$?
  if [ "${__status}" != 0 ]; then
    ret_env_var_set97_v0=''
    return "${__status}"
  fi
}

nix_config_option__120_v0() {
  local name=$1
  local values=("${!2}")
  join__8_v0 values[@] " "
  ret_join8_v0__5_23="${ret_join8_v0}"
  ret_nix_config_option120_v0="${name} = ${ret_join8_v0__5_23}"
  return 0
}

array_3=("nix-command" "flakes" "pipe-operators")
nix_config_option__120_v0 "experimental-features" array_3[@]
ret_nix_config_option120_v0__10_9="${ret_nix_config_option120_v0}"
array_4=("true")
nix_config_option__120_v0 "show-trace" array_4[@]
ret_nix_config_option120_v0__11_9="${ret_nix_config_option120_v0}"
nix_config_3=("${ret_nix_config_option120_v0__10_9}" "${ret_nix_config_option120_v0__11_9}")
join__8_v0 nix_config_3[@] "
"
ret_join8_v0__14_31="${ret_join8_v0}"
env_var_set__97_v0 "NIX_CONFIG" "${ret_join8_v0__14_31}"
__status=$?
if [ "${__status}" != 0 ]; then
  exit "${__status}"
fi
