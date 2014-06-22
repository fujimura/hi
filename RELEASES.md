# Releases

## 0.1.0 (Unreleased)

- Remove config file

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
