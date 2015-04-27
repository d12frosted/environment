#!/bin/sh

# todo - use MAILDIR variable

# Speak, friend, and enter.

FRIEND=$1
INTERVAL=$2

# but I speak only orcish

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

# gendalf activities

gendalf_think () {
    mbsync -a
}

gendalf_check () {
    for d in $(find ~/.mail -type d -and -name "Inbox" -or -name "Archive"); do
        for f in $(find $d -mmin -$INTERVAL -and -type f); do
            subject=$(mu view $f | grep '^Subject: ' | sed 's/^Subject: //')
            from=$(mu view $f | grep '^From: ' | sed 's/^From: //')
            if ! [ -z "$from" ]; then
                gendalf_mutter "$from" "$subject" "$f"
            fi
        done
    done
}

gendalf_mutter () {
    terminal-notifier -message "Subject: $2" \
                      -subtitle "From: $1" \
                      -title "New mail" \
                      -sender "org.gnu.Emacs" \
                      -activate "org.gnu.Emacs" \
                      -group "$3"
}

gendalf_enter () {
    mu index -m ~/.mail/
}

# gendalf figths

echo '****' $(date "+%Y-%m-%d% %H:%M:%S")

case $FRIEND in
    "mu4e")
        echo "     hello mu4e"
        gendalf_think
        gendalf_check
        ;;

    "saruman")
        echo "     hello saruman"
        gendalf_enter && gendalf_think && gendalf_check && gendalf_enter
        ;;

    "friend")
        echo "     the gate opens slowly"
        gendalf_check
        ;;

    *)
        ;;
esac

echo
