# Zext
Zext does not care about features. Zext does not care about auto-complete. Zext does not have a learning curve. Zext does not type for you. Zext opens your file with `zext file.txt`, takes in the little buttons you press, and when you press `Ctrl-s` it makes the file different. When Zext sees `esc` or `Ctrl-c` it fucks off and leaves you alone. Want to go up a line? Press the up arrow. Want to go down? Press the down arrow. Guess what the left and right arrows do?

Zext makes Nano look as complicated as Emacs.

## Compilation instructions
### Git clone or download this repo.
### Install curses for ocaml with opam.
### Run this command : `ocamlfind ocamlopt -o zext -linkpkg -package curses zext.ml`
### Now the file called `zext` can be run like this from your folder : `./zext ~/Path/To/File`
### Run this command to install zext: `sudo cp zext /usr/bin/zext`
