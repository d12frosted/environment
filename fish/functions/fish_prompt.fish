function __prompt_duration
  type __fish_duration_parser > /dev/null
  if test $status -eq 0
    set MIN_DURATION '8.00'
    set duration (__fish_duration_parser $CMD_DURATION)
    if test (echo $duration ' >= ' $MIN_DURATION | bc) -eq 1
      echo -es $white '~> duration: ' $yellow $duration 's' $normal
      if hash terminal-notifier ^ /dev/null
        echo -es 'Finished in ' $CMD_DURATION | terminal-notifier
      end
    end
  end
end

function __prompt_dir
  echo -ens $blue (pwd | sed 's:^$HOME:~:') $normal
end

function __prompt_git
  set -l git_branch (_git_branch_name)
  if test $git_branch
    set -l is_dirty (_git_is_dirty)
    set -l is_cherry (_git_is_cherry)

    echo -ens ' · ' '('

    # if dirty or contains commits that are not merged to upstream
    # print branch name in red
    if test $is_dirty -o $is_cherry
      echo -ens $red $git_branch
    else
      echo -ens $green $git_branch
    end

    # * - for dirty repository
    if test $is_dirty
      echo -ens ' *'
    end

    # @ - for repository with unmerged commits
    if test $is_cherry
      echo -ens ' @'
    end

    echo -ens $normal ')'

  end
end

function __prompt_lambda
  if test $RETVAL -ne 0
    echo -ens $red
  else
    echo -ens $green
  end
  echo -ens 'λ ' $normal
end

function fish_prompt
  set -g RETVAL $status

  __prompt_duration
  echo -e ''
  __prompt_dir
  __prompt_git
  echo -e ''
  __prompt_lambda
end
