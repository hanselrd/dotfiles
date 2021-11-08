let env = ../build/environment.dhall

in  ''
    [all]
    ${env.configuration} ansible_connection=local
    ''
