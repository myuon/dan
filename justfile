run: build
  cp ./.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin/dandan ./
  ./dandan --help

build:
  stack build
