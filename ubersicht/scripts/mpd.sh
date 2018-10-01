RESULTX="$(/usr/local/bin/mpc; echo x)"
RESULT="${RESULTX%x}"

 echo "$(echo "$RESULT" | grep - | awk -F"-" '{print $1}')@$(echo "$RESULT" | grep - | awk -F"-" '{print $2}')@$(echo "$RESULT" | grep / | awk -F" " '{print $4}' | grep -o '[0-9/]\+')@$(echo "$RESULT" | grep '\[' | awk -F" " '{print $1}')@$(echo "$RESULT" | grep / | awk -F" " '{print $2}' | grep -o '[0-9/]\+')@"
