hi [![Build Status](https://travis-ci.org/fujimura/hi.svg?branch=master)](https://travis-ci.org/fujimura/hi)[![Coverage Status](https://img.shields.io/coveralls/fujimura/hi.svg)](https://coveralls.io/r/fujimura/hi?branch=master)[![Hackage](https://img.shields.io/hackage/v/hi.svg)](http://hackage.haskell.org/package/hi)
===========

Generate scaffold for a Haskell project

## Overview

This application generates a scaffold for Haskell project from a Git repository.
The template can be specified by Git repository. [hi-hspec](https://github.com/fujimura/hi-hspec) will be used as a default template.

## Usage

### Basic Example

```
$ hi foo-bar-baz
Creating new project with git repository:git://github.com/fujimura/hi-hspec.git
    create  foo-bar-baz/.gitignore
    create  foo-bar-baz/LICENSE
    create  foo-bar-baz/README.md
    create  foo-bar-baz/foo-bar-baz.cabal
    create  foo-bar-baz/src/Foo/Bar/Baz.hs
    create  foo-bar-baz/src/Foo/Bar/Baz/Internal.hs
    create  foo-bar-baz/test/Foo/Bar/BazSpec.hs
    create  foo-bar-baz/test/Spec.hs
```

### Specifying template

Specifying repository in GitHub has a shorthand like this:

```
$ hi foo-bar-baz --repository gh:fujimura/hi-flat
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
  -h,--help                Show this help text
  -v,--version             Print version information
  -p,--package-name ARG    Name of package
  -m,--moduleName ARG      Name of Module
  -a,--author ARG          Name of the project's author
  -e,--email ARG           Email address of the maintainer
  -r,--repository ARG      Template repository
  --configuration-file ARG Use specified configuration file
  --initialize-git-repository
                           Initialize with git repository
  --after-command ARG      The command to be run after generation
```

## Templates

### Available templates

Please see https://github.com/fujimura/hi/wiki#available-templates and feel free to add yours!

### How to make your own template

[Template](http://hackage.haskell.org/package/template) is used for templating.

Available variables:

- `$packageName` : name of package, like 'hi'
- `$moduleName` : name of module, like 'Hi'
- `$author` : name of author, like 'Fujimura Daisuke'
- `$email` : email address of maintainer, like 'me@fujimuradaisuke.com'

'package-name' and 'ModuleName' in filepath will be replaced with given variable.

Files under `package-name` will be used as the source of generated files.
Files in root directory will not be copied.

## Motivation

I'm tired to organize directory structure for every new Haskell project which has some test.
This library is heavily inspired by [grunt-init](https://github.com/gruntjs/grunt-init).

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
