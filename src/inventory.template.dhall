let env = (../build/environment.dhall).default

in  ''
    [all]
    ${env.configuration} ansible_connection=local
    ''
