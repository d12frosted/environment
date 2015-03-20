function fish_prompt --description 'd12frosted prompt'
	set -l last_status $status

  __prompt_duration

  # PWD
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal

  __terlar_git_prompt
  __fish_hg_prompt
  # echo

  if not test $last_status -eq 0
    set_color $fish_color_error
  else
    set_color green
  end

  echo -n ' λ '
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
