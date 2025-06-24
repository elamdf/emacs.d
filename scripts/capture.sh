#!/bin/bash
/usr/local/opt/brew/bin/emacsclient -n -a "" --eval "(progn (raise-frame) (org-capture))" & 
osascript -e 'tell application "Emacs" to activate'
