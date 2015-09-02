#!/bin/sh

# todo - use MAILDIR variable

# Speak, friend, and enter.

FRIEND=$1
INTERVAL=$2

# but I speak only orcish
# so environment should support it

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

# gendalf activities

gendalf_think_fast () {
    mbsync d12frosted-inbox-channel boris-inbox-channel timecode-inbox-channel
}

gendalf_think_slow () {
    mbsync -a
}

gendal_push () {
    mbsync -a --push
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

threshold=1
index=0

gendalf_mutter () {
    index=$(expr $index + 1)
    if [ "$index" -gt "$threshold" ];
    then
        terminal-notifier -message "Subject: $2" \
                          -subtitle "From: $1" \
                          -title "New mail" \
                          -sender "org.gnu.Emacs" \
                          -activate "org.gnu.Emacs" \
                          -group "$3"
    else
        terminal-notifier -message "Subject: $2" \
                          -subtitle "From: $1" \
                          -title "New mail" \
                          -sender "org.gnu.Emacs" \
                          -activate "org.gnu.Emacs" \
                          -group "$3" \
                          -sound "Purr"
    fi
}

gendalf_index () {
    mu index -m ~/.mail/
}

# gendalf figths

echo '****' $(date "+%Y-%m-%d% %H:%M:%S")

case $FRIEND in
    "mu4e")
        echo "     hello mu4e"
        # gendalf_think_slow
        # gendalf_check
        ;;

    "saruman")
        echo "     hello saruman"
        gendalf_index && gendalf_think_slow && gendalf_check && gendalf_index
        ;;

    "melkor")
        echo "    Sing a song, Melkor!"
        gendalf_think_slow
        gendalf_index
        ;;

    "sauron")
        echo "    Dance to the song, Sauron!"
        gendalf_think_fast
        gendalf_check
        gendalf_index
        ;;

    "eru")
        echo "    Eru will guide you!"
        gendalf_push
        gendal_index
        ;;

    "friend")
        echo "     the gate opens slowly"
        gendalf_check
        ;;

    *)
        ;;
esac

echo
