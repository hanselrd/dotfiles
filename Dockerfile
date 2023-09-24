FROM redhat/ubi8

ARG USER=docker
ARG UID=1000
ARG GID=1000
ARG PRESET=linux-standard

ENV USER=${USER}

RUN groupadd --gid ${GID} --force ${USER}
RUN useradd --uid ${UID} --gid ${GID} --groups wheel --create-home ${USER}
RUN echo "${USER}:${USER}" | chpasswd

RUN dnf install \
    git \
    sudo \
    xz \
    -y

RUN mkdir -p /nix
RUN chown -R ${USER}:${GID} /nix

COPY --chown=${USER}:${GID} . /dotfiles

RUN git config --global --add safe.directory /dotfiles

WORKDIR /dotfiles

RUN git grep -l "delacruz" | xargs sed -i "s/delacruz/${USER}/g"

USER ${USER}

RUN ./scripts/nix-bootstrap.sh
RUN . /home/${USER}/.nix-profile/etc/profile.d/nix.sh && \
    nix run ".#dotfiles-home-manager" --accept-flake-config -- --preset ${PRESET}

WORKDIR /home/${USER}

ENTRYPOINT ["/bin/bash", "--login"]
