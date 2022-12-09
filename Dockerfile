FROM redhat/ubi8

ARG USER=docker
ARG UID=1000
ARG GID=1000

ENV USER=${USER}

RUN groupadd --gid ${GID} --force ${USER}
RUN useradd --uid ${UID} --gid ${GID} --create-home ${USER}
RUN echo "${USER}:${USER}" | chpasswd

RUN dnf install \
    git \
    xz \
    -y

RUN mkdir -p /nix
RUN chown -R ${USER}:${GID} /nix

COPY . /dotfiles

WORKDIR /dotfiles

RUN git grep -l "delacruz" | xargs sed -i "s/delacruz/${USER}/g"

USER ${USER}

RUN ./scripts/nix-bootstrap.sh
RUN ./scripts/home-manager-bootstrap.sh

WORKDIR /home/${USER}

ENTRYPOINT ["/bin/bash", "--login"]
