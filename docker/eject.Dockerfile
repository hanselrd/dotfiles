FROM redhat/ubi8

ARG USER=docker
ARG UID=1000
ARG GID=1000
ARG PROFILE=linux-base

ENV USER=${USER}
ENV HOME=/home/${USER}

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

COPY --chown=${USER}:${GID} . ${HOME}/.dotfiles

RUN git config --global --add safe.directory ${HOME}/.dotfiles

WORKDIR ${HOME}/.dotfiles

RUN sed -i "s/delacruz/${USER}/g" environment.go
RUN sed -i "/bashToZsh/s/false/true/g" environment.go

USER ${USER}

RUN ./scripts/nix-bootstrap.sh
RUN . ${HOME}/.nix-profile/etc/profile.d/nix.sh && \
    nix run ".#dotfiles-cli" -- homeManager bootstrap --profile ${PROFILE}
RUN . ${HOME}/.nix-profile/etc/profile.d/nix.sh && \
    nix run ".#dotfiles-cli" -- homeManager eject --profile ${PROFILE}

WORKDIR ${HOME}

ENTRYPOINT ["/bin/bash", "--login"]
