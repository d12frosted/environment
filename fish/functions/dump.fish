function dump -a target -d "Add timestamp to the end of target name"
  runhaskell "$XDG_CONFIG_HOME/fish/functions/helpers/dump.hs" $target
end
