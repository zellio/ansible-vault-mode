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

```bash
$ ls -al ~/.vault-pass
-rw------- 1 notroot notroot 33 May 18 16:11 /home/notroot/.vault-pass
```

Once that is set up we can start using `ansible-vault` normally.

The majority of my [ansible-vault][ansible-vault] files are called `encrypted`
and are [YAML][yaml] files so I have the following snippet to turn on ansible vault
when needed.

```lisp
(add-to-list 'auto-mode-alist '("/encrypted$" . yaml-mode))

(add-hook 'yaml-mode-hook
  (lambda ()
    (and (string= (file-name-base) "encrypted") (ansible-vault-mode 1))))
```

### Automatic enabling based on file contents

To enable `ansible-vault-mode` just based on the buffer contents, not on file extension, you can do:

```
  (defun ansible-vault-mode-maybe ()
    (when (ansible-vault--is-vault-file)
      (ansible-vault-mode 1)))

  (add-hook 'yaml-mode-hook 'ansible-vault-mode-maybe)
```

And if you use the handy `use-package` package replace the last line with:

```
  (use-package ansible-vault
    :init (add-hook 'yaml-mode-hook 'ansible-vault-mode-maybe))
```

### Per directory ansible-vault password file

To override ansible-vault password file on a per directory basis:
first, create a .dir-locals.el file in your directory:

```lisp
((yaml-mode
  (ansible-vault-pass-file . "/home/notroot/.ansible-vault/custom_vault_pass")))
```
then, if all your vaulted files are prefixed by "vault_", you can load
ansible-vault-mode in your init file this way:

```lisp
(add-hook 'hack-local-variables-hook
          (lambda ()
            (when (and
                   (derived-mode-p 'yaml-mode)
                   (string-prefix-p "vault_" (file-name-base)))
              (ansible-vault-mode 1))))
```

## Contributing

Bug reports and pull requests are welcome on [GitHub issues][issues]. This
project is intended to be a safe, welcoming space for collaboration, and
contributors are expected to adhere to the Contributor Covenant code of
conduct.

## Copyright

Copyright (C) 2016 Zachary Elliott &lt;contact@zell.io&gt;

```
This program is free software you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this software.  If not, write to the Free Software Foundation, Inc., 51
Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
```

[ansible-vault]: http://docs.ansible.com/ansible/playbooks_vault.html
[yaml]: http://yaml.org/
[issues]: https://github.com/zellio/ansible-vault-mode
