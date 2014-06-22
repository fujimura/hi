hi [![Build Status](https://travis-ci.org/fujimura/hi.png?branch=master)](https://travis-ci.org/fujimura/hi)[![Coverage Status](https://coveralls.io/repos/fujimura/hi/badge.png)](https://coveralls.io/r/fujimura/hi)
===========

Generate scaffold for a Haskell project

## Overview

This application generates a scaffold for Haskell project from a Git repository.
The template can be specified by Git repository. [hi-hspec](https://github.com/fujimura/hi-hspec) will be used as a default template.

## Usage

### Basic Example

```
$ hi --module-name "Foo.Bar.Baz" --author "Fujimura Daisuke" --email "me@fujimuradaisuke.com"
Creating new project from repository: git://github.com/fujimura/hi-hspec.git
    create  foo-bar-baz/.gitignore
    create  foo-bar-baz/LICENSE
    create  foo-bar-baz/README.md
    create  foo-bar-baz/foo-bar-baz.cabal
    create  foo-bar-baz/src/Foo/Bar/Baz.hs
    create  foo-bar-baz/src/Foo/Bar/Baz/Internal.hs
    create  foo-bar-baz/test/Foo/Bar/BazSpec.hs
    create  foo-bar-baz/test/Spec.hs

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

### Specifying template

Specifying repository in GitHub has a shorthand like this:

```
$ ./dist/build/Hi/hi --module-name "Foo.Bar.Baz" --repository gh:fujimura/hi-flat
Creating new project from repository: git@github.com:fujimura/hi-flat.git
    create  foo-bar-baz/.gitignore
    create  foo-bar-baz/LICENSE
    create  foo-bar-baz/Main.hs
    create  foo-bar-baz/Foo/Bar/Baz.hs
    create  foo-bar-baz/README.md
    create  foo-bar-baz/foo-bar-baz.cabal
```

## Options

```
$ hi --help
hi: Usage: hi [OPTION...]
Generate a haskell project based on a template from github.

  -m Module.Name   --module-name=Module.Name        Name of Module
  -p package-name  --package-name=package-name      Name of package        ( optional )
  -a NAME          --author=NAME                    Name of the project's author
  -e EMAIL         --email=EMAIL                    Email address of the maintainer
  -r REPOSITORY    --repository=REPOSITORY          Template repository    ( optional )
  -v               --version                        Show version number
                   --initialize-git-repository      Initialize with git repository
  -h               --help                           Display this help and exit

If repository is not provided, it defaults to the repository at
git://github.com/fujimura/hi-hspec.git.

Example:
    hi --module-name 'Foo.Bar' --author 'you' --email 'you@gmail.com'
```

## Templates

### Available templates at this moment

- [hi-hspec](https://github.com/fujimura/hi-hspec) : Sources in `src`, tests in `test` by [Hspec](https://github.com/hspec/hspec) .

- [hi-flat](https://github.com/fujimura/hi-flat) : Everything is in root directory, like `cabal init`.

Please let me know if you have your own template. I'll add it here.

### How to make your own template

[Template](http://hackage.haskell.org/package/template) is used for templating.

Available variables:

- `$packageName` : name of package, like 'hi'
- `$moduleName` : name of module, like 'Hi'
- `$author` : name of author, like 'Fujimura Daisuke'
- `$email` : email address of maintainer, like 'me@fujimuradaisuke.com'

'package-name' and 'ModuleName' in filepath will be replaced with given variable.

## Motivation

I'm tired to organize directory structure for every new Haskell project which has some test.
This library is heavily inspired by [grunt-init](https://github.com/gruntjs/grunt-init).

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
