hi [![Build Status](https://travis-ci.org/fujimura/hi.png?branch=master)](https://travis-ci.org/fujimura/hi)
===========

Generate scaffold for a Haskell project

## Overview

This application generates a scaffold for Haskell project from a Git repository.
[hi-hspec](https://github.com/fujimura/hi-hspec) will be used as a default template.


## How it works

```
$ hi --package-name "foo-bar-baz" --module-name "Foo.Bar.Baz" --author "Fujimura Daisuke" --email "me@fujimuradaisuke.com"
"
$ tree .
.
├── LICENSE
├── README.md
├── foo-bar-baz.cabal
├── src
│   └── Foo
│       └── Bar
│           ├── Baz
│           │   └── Internal.hs
│           └── Baz.hs
└── test
    ├── Foo
    │   └── Bar
    │       ├── Baz
    │       └── BazSpec.hs
    └── Spec.hs

8 directories, 7 files
```

## Usage

```
hi: Usage: hi [OPTION...]
  -p package-name  --package-name=package-name  Name of package
  -m Module.Name   --module-name=Module.Name    Name of Module
  -a NAME          --author=NAME                Name of the project's author
  -e EMAIL         --email=EMAIL                Email address of the maintainer
  -r REPOSITORY    --repository=REPOSITORY      Template repository(optional)
  -v               --version                    show version number
```

## Installation

```
$ cabal install hi
```

## Making your own project template

TODO

[Template](http://hackage.haskell.org/package/template) is used for templating.

## Motivation

I'm tired to organize directory structure of every new Haskell project which comes with some test.
This library is heavily inspired by [grunt-init](https://github.com/gruntjs/grunt-init).

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
