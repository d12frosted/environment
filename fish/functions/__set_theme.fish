function __set_theme -a theme -d "Set fish prompt color theme"
  switch $theme

    case "spacemacs_light"
      set fish_color_command blue
      set fish_color_param 715ab1
      set fish_color_autosuggestion 9f8fbd
      set fish_color_operator 428374
      set fish_color_end 8959a8
      set fish_color_error e0211d
      set fish_color_comment 2aa198
      set fish_color_quote 718c00
      set fish_color_redirection 3e999f

    case "tomorrow"
      # .editor
      set fish_color_normal 4d4d4c
      # .entity.name.function
      set fish_color_command 4271ae
      # .variable.parameter
      set fish_color_quote f5871f
      # .keyword
      set fish_color_redirection 8959a8
      # .keyword
      set fish_color_end 8959a8
      # .variable
      set fish_color_error c82829
      # .variable.parameter
      set fish_color_param f5871f
      # .comment
      set fish_color_comment 8e908c
      # .keyword.operator
      set fish_color_match 3e999f
      # .keyword
      set fish_color_search_match 8959a8
      # .keyword.operator
      set fish_color_operator 3e999f
      # .keyword.operator
      set fish_color_escape 3e999f
      # .string
      set fish_color_cwd 718c00

    case "tomorrow_night"
      # .editor
      set fish_color_normal c5c8c6
      # .entity.name.function
      set fish_color_command 81a2be
      # .variable.parameter
      set fish_color_quote de935f
      # .keyword
      set fish_color_redirection b294bb
      # .keyword
      set fish_color_end b294bb
      # .variable
      set fish_color_error cc6666
      # .variable.parameter
      set fish_color_param de935f
      # .comment
      set fish_color_comment 969896
      # .keyword.operator
      set fish_color_match 8abeb7
      # .keyword
      set fish_color_search_match b294bb
      # .keyword.operator
      set fish_color_operator 8abeb7
      # .keyword.operator
      set fish_color_escape 8abeb7
      # .string
      set fish_color_cwd b5bd68

    case "tomorrow_night_bright"
      # .editor
      set fish_color_normal dedede
      # .entity.name.function
      set fish_color_command 7aa6da
      # .variable.parameter
      set fish_color_quote e78c45
      # .keyword
      set fish_color_redirection c397d8
      # .keyword
      set fish_color_end c397d8
      # .variable
      set fish_color_error d54e53
      # .variable.parameter
      set fish_color_param e78c45
      # .comment
      set fish_color_comment 969896
      # .keyword.operator
      set fish_color_match 70c0b1
      # .keyword
      set fish_color_search_match c397d8
      # .keyword.operator
      set fish_color_operator 70c0b1
      # .keyword.operator
      set fish_color_escape 70c0b1
      # .string
      set fish_color_cwd b9ca4a

    # case "tomorrow_night_eighties"

    case '*'
      set_color $fish_color_error
      echo
      echo "Error: Unknown theme '$theme'"
      echo
      set_color normal
  end
end
