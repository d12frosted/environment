function fish_prompt --description 'Write out the prompt'
	set -l last_status $status

  __prompt_duration

  echo

  # Time
  set_color $fish_color_comment
  echo -ns "[" (date +%X) "] "

  # User
  set_color $fish_color_user
  echo -n (whoami)
  set_color normal

  echo -n '@'

  # Host
  set_color $fish_color_host
  echo -n (hostname -s)
  set_color normal

  echo -n ':'

  # PWD
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal

  __fish_venv
  __terlar_git_prompt
  __fish_hg_prompt
  echo

  if not test $last_status -eq 0
    set_color $fish_color_error
  end

  # echo -n '➤ '
  set_color $fish_color_operator
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
end

function __fish_venv
  if set -q VIRTUAL_ENV
    echo -n -s "|" (set_color $fish_color_comment) (basename "$VIRTUAL_ENV") (set_color normal)
  end
end
