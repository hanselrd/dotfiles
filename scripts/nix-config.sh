#!/bin/sh
export genspio_trap_4_45453=$$
trap 'exit 77' USR1
export $(printf -- "$(printf -- '%s\n' "$(
  byte_array_to_c_string_6_99327=116111130137103117116106111107
  if [ "$(printf -- "$byte_array_to_c_string_6_99327\n" | sed -e 's/\(.\{3\}\)/@\1/g' | grep @000)" = "" ]; then printf -- "$byte_array_to_c_string_6_99327"; else {
    printf -- '%s\n' 'Error:
Byte-array cannot be converted to a C-string:
{variable: byte_array_to_c_string_6_99327; content:
  116111130137103117116106111107; code: (string "NIX_CONFIG")}' >&2
    kill -s USR1 ${genspio_trap_4_45453}
  }; fi
)" | sed -e 's/\(.\{3\}\)/\\\1/g')")="$(printf -- "$(printf -- '%s\n' "$(
  byte_array_to_c_string_5_30731=145170160145162151155145156164141154055146145141164165162145163040075040156151170055143157155155141156144040146154141153145163040160151160145055157160145162141164157162163012163150157167055164162141143145040075040164162165145
  if [ "$(printf -- "$byte_array_to_c_string_5_30731\n" | sed -e 's/\(.\{3\}\)/@\1/g' | grep @000)" = "" ]; then printf -- "$byte_array_to_c_string_5_30731"; else {
    printf -- '%s\n' 'Error:
Byte-array cannot be converted to a C-string:
{variable: byte_array_to_c_string_5_30731; content:
  14517016014516215115514515616414115405514614514116416516214516304 …;
  code:
  (string
"experimental-features = nix-command flakes pipe-operator …}' >&2
    kill -s USR1 ${genspio_trap_4_45453}
  }; fi
)" | sed -e 's/\(.\{3\}\)/\\\1/g')")"
