# cwis-cli

A CLI interface for CentreWare Internet Services from Fuji Xerox.

Tested with ApeosPort-V C5585.

# Install

```bash
$ git clone https://github.com/rinse/cwis-cli.git && cd cwis-cli
$ stack install
```

# Usage

Show help

```bash
$ cwis-cli-exe --help
Usage: cwis-cli-exe --hostname ARG [-n|--copies ARG] (-u|--username ARG)
                    (-p|--password ARG) FILE
  This is a cli interface for a printer.

Available options:
  -h,--help                Show this help text
  --hostname ARG           the hostname of the printer
  -n,--copies ARG          the number of copies (default: 1)
  -u,--username ARG        the username for security print
  -p,--password ARG        the password for security print
  FILE                     the file to print
```

I recommend you to have an alias like:

```bash
alias cwis-cli-exe='cwis-cli-exe --hostname hostname -u username -p password'
```

Or, add an item to the right-click menu like:

```desktop
[Desktop Entry]
Type=Action
Name=Send To Printer
Profiles=print;

[X-Action-Profile print]
Name=Default profile
MimeTypes=application/pdf
Exec=cwis-cli-exe --hostname hostname -u username -p password '%f'
```
