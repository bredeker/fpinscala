# syntax=docker/dockerfile:1.3-labs

FROM mcr.microsoft.com/devcontainers/base:ubuntu

ARG BUILDARCH

RUN <<EOF_APT
  apt-get update
  apt-get install -y \
    openjdk-17-jdk
  rm -rf /var/lib/apt/lists/*
EOF_APT

RUN <<EOF_CS
  su - vscode -c 'mkdir -p $HOME/.local/bin'
  # Coursier JAR launcher
  su - vscode -c 'curl -fL coursier https://github.com/coursier/launchers/raw/master/coursier > $HOME/.local/bin/coursier'
  su - vscode -c 'chmod +x $HOME/.local/bin/coursier && $HOME/.local/bin/coursier setup --yes'
  # Coursier native launcher
  case $BUILDARCH in
    "arm64")
      su - vscode -c 'curl -fL "https://github.com/VirtusLab/coursier-m1/releases/latest/download/cs-aarch64-pc-linux.gz" | gzip -d > $HOME/.local/bin/cs'
      ;;
    "amd64")
      su - vscode -c 'curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > $HOME/.local/bin/cs'
      ;;
    *)
      echo "Unsupported architecture $BUILDARCH for Coursier native launcher" > ~vscode/.cs_install_aborted
      ;;
  esac
  su - vscode -c '[ -e $HOME/.local/bin/cs ] && chmod +x $HOME/.local/bin/cs && $HOME/.local/bin/cs setup --yes'
  su - vscode -c '[ -e $HOME/.local/bin/cs ] || ln -s $HOME/.local/bin/coursier $HOME/.local/bin/cs'
  su - vscode -c 'echo "export JAVA_HOME=$($HOME/.local/bin/cs java-home)" >> $HOME/.profile'
  su - vscode -c '$HOME/.local/bin/cs launch scala-cli -- clean'
  su - vscode -c '$HOME/.local/bin/cs launch scala-cli -- repl --repl-dry-run'
EOF_CS
