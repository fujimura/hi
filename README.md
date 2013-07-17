boilerplate
===========

Generate scaffold for a Haskell project

## Overview

This application generates a scaffold for Haskell project from a Git repository.

## How to use

```
boilerplate: Usage: boilerplate [OPTION...]
  -p package-name  --package-name=package-name  Name of package
  -m Module.Name   --module-name=Module.Name    Name of Module
  -a NAME          --author=NAME                Name of the project's author
  -e EMAIL         --email=EMAIL                Email address of the maintainer
  -r REPOSITORY    --repository=REPOSITORY      Template repository
  -v               --version                    show version number
```

## Installation

```
$ cabal install boilerplate
```

## Making your own project template

TODO

[Template](http://hackage.haskell.org/package/template) is used for templating.

## Motivation

TODO

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
