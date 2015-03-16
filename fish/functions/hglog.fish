function hglog
  set -l template (echo -es '\t\033[38;5;' $green '{node|short} \033[38;5;' $purple '{fill(sub("^(.{10}).*$","\\1",branch),"10")} \033[38;5;' $yellow '{date|isodatesec} \033[38;5;' $red '{author|user}: \033[38;5;' $normal '{desc|strip|firstline}\033[m\n')

  set -l highlight (echo -e '\033[48;5;19m\033[38;5;256m')

  hg log -G --template "$template" -l 15 * | sed "s/@/$highlight&/g"
end
