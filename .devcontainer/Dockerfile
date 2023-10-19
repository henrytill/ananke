FROM mcr.microsoft.com/devcontainers/base:debian

ARG USERNAME=vscode

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -qq && \
    apt-get install -yqq \
    build-essential libc6-dev libffi-dev libgmp-dev libnuma-dev libtinfo-dev zlib1g-dev

# Install latest GHCup in the non-root user home
USER $USERNAME

RUN mkdir -p "$HOME/.ghcup/bin" && \
    curl -LJ "https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup" -o "$HOME/.ghcup/bin/ghcup" && \
    chmod +x "$HOME/.ghcup/bin/ghcup"

ENV PATH="/home/$USERNAME/.cabal/bin:/home/$USERNAME/.ghcup/bin:$PATH"

ARG GHC_VERSION="recommended"

ARG CABAL_VERSION="3.10"

RUN ghcup install ghc "${GHC_VERSION}" --set && \
    ghcup install cabal "${CABAL_VERSION}" --set && \
    ghcup install hls recommended --set && \
    cabal update
