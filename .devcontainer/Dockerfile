FROM mcr.microsoft.com/devcontainers/base:ubuntu

RUN su - vscode -c 'curl -sSLf https://virtuslab.github.io/scala-cli-packages/scala-setup.sh | sh'

# The scala-cli Docker Hub image runs the following to pre-init things
# /bin/sh -c 'echo "println(1)" | scala-cli -S 3 - -v -v -v &&  echo "println(1)" | scala-cli -S 2.13 - -v -v -v &&  echo "println(1)" | scala-cli -S 2.12 - -v -v -v'
