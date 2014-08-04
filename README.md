# Cobol unit testing framework (batch programs)

This is a unit testing framework for batch Cobol programs. The goal is to enable isolated unit testing of individual paragraphs in Cobol programs in a standalone development environment with no connection to a zOS system.

This depends on GNU Cobol.

If you are loading this on a system configured using provision-cobol-dev-ubuntu, then clone this repo as follows:

```shell
cd ~/projects
clone https://github.com/neopragma/cobol-unit-test
```

Run the build script like this:

```shell
cd ~/projects/cobol-unit-test
source envvars
build
``` 