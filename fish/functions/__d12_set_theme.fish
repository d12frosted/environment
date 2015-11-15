function __d12_set_theme -a theme -d "Set fish prompt color theme"
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
      set fish_color_normal 4d4d4c
      set fish_color_command 4271ae
      set fish_color_quote b5bd68
      set fish_color_redirection 8959a8
      set fish_color_end 8959a8
      set fish_color_error c82829
      set fish_color_param f5871f
      set fish_color_comment 8e908c
      set fish_color_match 3e999f
      set fish_color_search_match 8959a8
      set fish_color_operator 3e999f
      set fish_color_escape 3e999f
      set fish_color_cwd 718c00

    case "tomorrow_night"
      set fish_color_autosuggestion 969896
      set fish_color_command b294bb
      set fish_color_param 81a2be
      set fish_color_redirection 8abeb7
      set fish_color_comment f0c674
      set fish_color_operator 81a2be
      set fish_color_error cc6666
      set fish_color_quote b5bd68
      set fish_color_end b294bb

    case "tomorrow_night_bright"
      set fish_color_normal dedede
      set fish_color_command 7aa6da
      set fish_color_quote e78c45
      set fish_color_redirection c397d8
      set fish_color_end c397d8
      set fish_color_error d54e53
      set fish_color_param e78c45
      set fish_color_comment 969896
      set fish_color_match 70c0b1
      set fish_color_search_match c397d8
      set fish_color_operator 70c0b1
      set fish_color_escape 70c0b1
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
