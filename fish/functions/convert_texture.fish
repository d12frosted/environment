function convert_texture -a input_file output_format -d "Convert texture to specified format using Texture Packer"
  hash TexturePacker > /dev/null 2>&1; or begin
    echo "Error! You don't have TexturePacker command line tools! Install them first!"
    return 1
  end

  if test (count $argv) -ne 2
    echo "You must provide INPUT_FILE and OUTPUT_FORMAT"
    return 1
  end

  # process input
  set -l output_file (echo $input_file | sed "s/\([a-zA-Z_1-9 ]*\)\..*/\1.$output_format/")
  set -l data_file $output_file.plist

  TexturePacker $input_file --sheet $output_file --data $data_file --algorithm Basic --allow-free-size --no-trim --padding 0
  rm $data_file
end
