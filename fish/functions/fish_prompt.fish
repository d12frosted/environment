function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

  __d12_prompt__check_duration

  echo

  if test $last_status -ne 0
    set_color $fish_color_error
    printf "<%d> " $last_status
  end

  # Time
  set_color $fish_color_comment
  printf "[%s]" (date +%X)

  # User and host
  set -l user (whoami)
  set -l host (hostname | cut -d . -f 1)
  set -l default_user "d12frosted"
  set -l default_host "d12frosted"
  if test "$user" != $default_user
    set_color $fish_color_user
    printf " $user"
  end
  if test "$host" != $default_host
    set_color $fish_color_comment
    if test "$user" = $default_user
      printf " "
    end
    printf "@"
    set_color $fish_color_host
    printf "$host"
  end

  # PWD
  set_color $fish_color_cwd
  printf " %s" (prompt_pwd)

  set_color normal

  if command -v git > /dev/null
    __terlar_git_prompt
  end

  echo

  if test $last_status -ne 0
    set_color $fish_color_error
  else
    set_color $fish_color_operator
  end

  # echo -n '➤ '
  if test "$TERM" = "linux"
    echo -n '> '
  else
    echo -n 'λ '
  end
  set_color normal
end

function __d12_prompt__check_duration
  if test $CMD_DURATION
    if test $CMD_NOTIFICATION_THRESHOLD
      if test $CMD_DURATION -ge $CMD_NOTIFICATION_THRESHOLD
        __d12_prompt__on_duration_exceeded $CMD_DURATION
        __d12_prompt__notify_completion $CMD_DURATION
      end
    end
  end
  set CMD_DURATION 0
end

function __d12_prompt__on_duration_exceeded -a duration
  set_color $fish_color_command
  echo -esn '  ~> duration: '
  set_color $fish_color_param
  echo -es $duration ' ms'
  set_color normal
end

function __d12_prompt__notify_completion -a duration
  set last_cmd (echo $history[1] | cut -d ' ' -f 1)
  notify send -a "Terminal" -t $last_cmd -m "Finished in $duration ms" -u low
end
