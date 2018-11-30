#!/usr/bin/env bash

printf "#!/usr/bin/env bash\n\n" > vscode_extensions.sh && code --list-extensions | xargs -L 1 echo "code --install-extension" >> vscode_extensions.sh
chmod +x vscode_extensions.sh
