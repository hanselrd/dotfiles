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

COPY --chown=${USER}:${GID} . ${HOME}/.dotfiles

RUN git config --global --add safe.directory ${HOME}/.dotfiles

WORKDIR ${HOME}/.dotfiles

RUN sed -i "s/delacruz/${USER}/g" pkg/environment/environment.go
RUN sed -i "/bashToZsh/s/false/true/g" pkg/environment/environment.go

USER ${USER}

RUN mkdir -p ${HOME}/.nix

RUN ./scripts/proot-bootstrap.sh
RUN ${HOME}/.bootstrap/proot -b ${HOME}/.nix:/nix ./scripts/nix-bootstrap.sh
# RUN ${HOME}/.bootstrap/proot -b ${HOME}/.nix:/nix /bin/bash -c ". ${HOME}/.nix-profile/etc/profile.d/nix.sh && \
#     nix run .#dotfiles-cli -- home-manager bootstrap --profile ${PROFILE}"

WORKDIR ${HOME}

# ENTRYPOINT ${HOME}/.bootstrap/proot -b ${HOME}/.nix:/nix /bin/bash --login
ENTRYPOINT ["/bin/bash", "--login"]
