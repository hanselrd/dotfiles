FROM nixos/nix

ENV NIX_CONFIG="experimental-features = nix-command flakes pipe-operators"
ENV HOME=/root

COPY . $HOME/.dotfiles

WORKDIR $HOME/.dotfiles

RUN nix develop -c sh -c " \
  nix profile remove \
    bash-interactive \
    coreutils-full \
    curl \
    findutils \
    git-minimal \
    gnugrep \
    gnutar \
    gzip \
    less \
    man-db \
    nix \
    wget \
    which \
  && { nh home switch -c docker -b bak || true; } \
"

ENTRYPOINT ["/bin/sh", "-l"]
