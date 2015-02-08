function find_dirty
	set currentDir (pwd)
  set repos (find $HOME -name .git -type d)
  for val in $repos
    cd $val/..
    set isDirty (_git_is_dirty)
    set isUnpushed (_git_is_cherry)
    if [ $isDirty ]
      if test $isUnpushed -ne 0
        echo -e -s $green (pwd) $yellow " is dirty and has unpushed commits" $normal
      else
        echo -e -s $green (pwd) $yellow " is dirty" $normal
      end
    else
      if test $isUnpushed -ne 0
        echo -e -s $green (pwd) $yellow " has unpushed commits" $normal
      end
    end
  end
  cd $currentDir
end
