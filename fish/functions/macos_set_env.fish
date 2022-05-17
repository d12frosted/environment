# Adapt construct_path from the macOS /usr/libexec/path_helper executable for
# fish usage;
#
# The main difference is that it allows to control how extra entries are
# preserved: either at the beginning of the VAR list or at the end via first
# argument MODE.
#
# Usage:
#
#   __fish_macos_set_env MODE VAR VAR-FILE VAR-DIR
#
#   MODE: either append or prepend
#
# Example:
#
#   __fish_macos_set_env prepend PATH /etc/paths '/etc/paths.d'
#
#   __fish_macos_set_env append MANPATH /etc/manpaths '/etc/manpaths.d'
#
# [1]: https://opensource.apple.com/source/shell_cmds/shell_cmds-203/path_helper/path_helper.c.auto.html .
#
function macos_set_env -d "set an environment variable like path_helper does (macOS only)"
  # noops on other operating systems
  if test $KERNEL_NAME darwin
    set -l result
    set -l entries

    # echo "1. $argv[2] = $$argv[2]"

    # Populate path according to config files
    for path_file in $argv[3] $argv[4]/*
      if [ -f $path_file ]
        while read -l entry
          if not contains -- $entry $result
            test -n "$entry"
            and set -a result $entry
          end
        end <$path_file
      end
    end

    # echo "2. $argv[2] = $result"

    # Merge in any existing path elements
    set entries $$argv[2]
    if test $argv[1] = "prepend"
      set entries[-1..1] $entries
    end
    for existing_entry in $entries
      if not contains -- $existing_entry $result
        if test $argv[1] = "prepend"
          set -p result $existing_entry
        else
          set -a result $existing_entry
        end
      end
    end

    # echo "3. $argv[2] = $result"

    set -xg $argv[2] $result
  end
end
