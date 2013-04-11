
# Have to base64-encode this, since sh on windows treats ':' character as some kind of escape sequence
ELISP_DIR=$(emacs --batch -q --no-site-file --eval "(message (base64-encode-string (expand-file-name \"elisp\")))" 2>&1)

cd elisp/magit
make test EFLAGS="--eval \"(add-to-list 'load-path (base64-decode-string \\\"$ELISP_DIR\\\") t)\""
