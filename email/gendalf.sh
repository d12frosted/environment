#!/bin/sh

MAILDIR="~/.mail"

gendalf_search () {
    for d in $(find ~/.mail -type d -and -name "INBOX" -or -name "ARCHIVE"); do
        for f in $(find $d -mmin -1 -and -type f); do
            subject=$(mu view $f | grep '^Subject: ' | sed 's/^Subject: //')
            from=$(mu view $f | grep '^From: ' | sed 's/^From: //')
            if ! [ -z "$from" ]; then
                gendalf_msg "$from" "$subject"
            fi
        done
    done
}

gendalf_msg () {
    terminal-notifier -message "Subject: $2" \
                      -subtitle "From: $1" \
                      -title "New mail" \
                      -sender "org.gnu.Emacs" \
                      -activate "org.gnu.Emacs"
}

mbsync -a
gendalf_search
mu index -m ~/.mail
