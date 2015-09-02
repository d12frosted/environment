function fish_prompt --description 'Write out the prompt'
	set -l last_status $status

  __prompt_duration

  echo

  if test $last_status -ne 0
    set_color $fish_color_error
    printf "<%d> " $last_status
  end

  # Time
  set_color $fish_color_comment
  printf "[%s] " (date +%X)

  # PWD
  set_color $fish_color_cwd
  printf "%s" (prompt_pwd)

  set_color normal

  __fish_venv
  __terlar_git_prompt
  __fish_hg_prompt

  echo

  if test $last_status -ne 0
    set_color $fish_color_error
  else
    set_color $fish_color_operator
  end

  # echo -n '➤ '
  echo -n 'λ '
  set_color normal
end

function __prompt_duration
  if test $CMD_DURATION
    if test $CMD_DURATION -ge 8000
      set_color white
      echo -esn '  ~> duration: '
      set_color yellow
      echo -es $CMD_DURATION ' ms'
      set_color normal
      if hash terminal-notifier ^ /dev/null
        echo -es 'Finished in ' $CMD_DURATION ' ms' | terminal-notifier
      end
    end
  end
  set CMD_DURATION 0
end

function __fish_venv
  if set -q VIRTUAL_ENV
    echo -n -s "|" (set_color $fish_color_comment) (basename "$VIRTUAL_ENV") (set_color normal)
  end
end
