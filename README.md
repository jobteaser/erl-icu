
# erl-icu

## Introduction
The repository contains an OTP library application which binds the [ICU
library](http://site.icu-project.org).

The `icu_nif` module is a low level binding which matches C functions as much
as possible.

Other modules provide higher level functions.

## Build
The project can be built with:

```sh
    make
```

The ICU library must be installed.

## Tests
Tests can be run with:

```sh
    make test
```

## Documentation
Code documentation is available at
[https://jobteaser.github.io/erl-icu/doc](https://jobteaser.github.io/erl-icu/doc).
