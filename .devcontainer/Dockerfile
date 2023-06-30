# syntax=docker/dockerfile:1.3-labs

FROM mcr.microsoft.com/devcontainers/base:ubuntu

ARG BUILDARCH

RUN <<EOF_APT
  export DEBIAN_FRONTEND=noninteractive
  apt update && apt upgrade -y
  [ -d /etc/apt/keyrings ] || mkdir -p /etc/apt/keyrings
  curl -fSLo /etc/apt/keyrings/adoptium.asc https://packages.adoptium.net/artifactory/api/gpg/key/public
  echo "deb [signed-by=/etc/apt/keyrings/adoptium.asc] https://packages.adoptium.net/artifactory/deb $(awk -F= '/^UBUNTU_CODENAME/{print$2}' /etc/os-release) main" > /etc/apt/sources.list.d/adoptium.list
  apt update
  apt install -y --no-install-recommends \
    temurin-17-jdk
  rm -rf /var/lib/apt/lists/*
EOF_APT

RUN <<EOF_CS
  su - vscode -c '[ -d $HOME/.local/bin ] || mkdir -p $HOME/.local/bin'
  # Coursier JAR launcher
  su - vscode -c 'curl -fSLo $HOME/.local/bin/coursier https://github.com/coursier/launchers/raw/master/coursier'
  su - vscode -c 'chmod +x $HOME/.local/bin/coursier && $HOME/.local/bin/coursier setup --yes'
  # Coursier native launcher
  case $BUILDARCH in
    "arm64")
      su - vscode -c 'curl -fSL "https://github.com/VirtusLab/coursier-m1/releases/latest/download/cs-aarch64-pc-linux.gz" | gzip -d > $HOME/.local/bin/cs'
      ;;
    "amd64")
      su - vscode -c 'curl -fSL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > $HOME/.local/bin/cs'
      ;;
    *)
      echo "Unsupported architecture $BUILDARCH for Coursier native launcher" > ~vscode/.cs_install_aborted
      ;;
  esac
  su - vscode -c '[ -e $HOME/.local/bin/cs ] && chmod +x $HOME/.local/bin/cs && $HOME/.local/bin/cs setup --yes'
  su - vscode -c '[ -e $HOME/.local/bin/cs ] || ln -s $HOME/.local/bin/coursier $HOME/.local/bin/cs'
  su - vscode -c 'echo "export JAVA_HOME=$($HOME/.local/bin/cs java-home)" >> $HOME/.profile'
  su - vscode -c '$HOME/.local/bin/cs launch scala-cli -- repl --repl-dry-run'
EOF_CS
