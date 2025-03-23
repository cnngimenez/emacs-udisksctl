# Emacs interface for udisksctl

# Requirements
Any Operative System with a working `udisksctl` command.

# Usage
Call `M-x udisksctl-list` to see the list of devices plugged on your computer, with their associated directories. The buffer is in udisksctl-mode and provides a keymap.

The most important interactive functions are:

- `udisksctl-list`
- `udisksctl-status`
- `udisksctl-info`
- `udisksctl-mount`
- `udisksctl-unmount`
- `udisksctl-unlock`
- `udisksctl-lock`
- `udisksctl-poweroff`

## Auth-source
For locked (i.e. encrypted) disks, passwords are obtained from auth-source or directly from user prompt. If `udisksctl-use-auth-source-passwords` customisation variable is nil, then auth-source is not used.

# License
![GPLv3 Logo](https://www.gnu.org/graphics/gplv3-with-text-136x68.png)
This works is under the GNU General Public License version 3.

