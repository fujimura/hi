# Releases

## 1.1.0.1

- Fix version of process in executable(the change in 1.1.0.0 is only in library)

## 1.1.0.0

- Added `--after-command` option to run arbitrary command after generation
- Fix version of process

## 1.0.0.0

- Files will be generated in `package-name` directory
  - If specified template has `package-name` directory in its root, it will be ignored.
  - Fixes https://github.com/fujimura/hi/issues/26
- Non-template file will be ignored if template file with same name exists
  - Fixes https://github.com/fujimura/hi/issues/34 along with fix above
- `package-name` is a must, `module-name` is optional
- `package-name` can be specified as argument

## 0.0.8.2

- Only convert template files to text to allow non-textual files to be included in a template repo #38 (Thanks @timmytofu)

## 0.0.8.1

- Fix dependencies

## 0.0.8

- File in the template which doesn't end with `.template` will be copied without argument substution (Thanks @Zane-XY-W)

## 0.0.7

- `--no-configuration-file` option was removed.
- More verbose program usage (Thanks @cdepillabout)
- `--help` option (Thanks @cdepillabout)
- `--initialize-git-repository` option to initialize generated files as git repository
- Add shorthand to specify repository in GitHub(`gh:foo/bar`)
- If package name was not given, use hyphenized module name

## 0.0.6

- Fix "Hidden file in template is ignored" ([#10](https://github.com/fujimura/hi/pull/10))

## 0.0.5

- Output logs to stdout

## 0.0.4

- Stop generating `src` and `test` directory if it doesn't exist in template

## 0.0.3

- Fix "Hi crashes when $HOME/.hirc doesn't exist" ([#8](https://github.com/fujimura/hi/issues/8))

## 0.0.2

- Add configuration file

## 0.0.1

- Initial version
