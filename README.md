# ansible-vault-mode

Minor mode for in place manipulation of [ansible-vault][ansible-vault].

## Installation

You can install via ELPA:

```
M-x package-install RET ansible-vault RET
```

Or manually downloading `ansible-vault-mode` and adding the following lines to
your conf files:

```lisp
(add-to-list 'load-path "/path/to/ansible-vault")
(require 'ansible-vault-mode)
```

## Usage

Once `ansible-vault-mode` in installed you will need to do a little
configuring before it is useful.

First you will need to set up your ansible-vault password file. By default
`ansible-vault-mode` assumes the file is located at `~/.vault-pass`. Either
put your password there or customize the mode to change the location.

You will want to make sure that file has the mode `0600` so other people
cannot read it.

```
$ ls -al ~/.vault-pass
-rw------- 1 notroot notroot 33 May 18 16:11 /home/notroot/.vault-pass
```

Once that is set up we can start using `ansible-vault` normally.

The majority of my [ansible-vault][ansible-vault] files are called `encrypted`
and are [YAML][yaml] files so I have the following snippet to turn on ansible vault
when needed.

```
(add-to-list 'auto-mode-alist '("/encrypted$" . yaml-mode))

(add-hook 'yaml-mode-hook
  (lambda ()
    (and (string= (file-name-base) "encrypted") (ansible-vault-mode 1))))

```

[ansible-vault]:http://docs.ansible.com/ansible/playbooks_vault.html
[yaml]:[http://yaml.org/]
