function mov_to_gif -a file -d "Convert .mov file to .gif"
  # process input
  set -l out (echo $file | sed 's/\(.*\)\..*/\1.gif/')

  # make sure that everything in installed
  __package_install ffmpeg
  __package_install gifsicle

  # just do the work
  ffmpeg -i $file -pix_fmt rgb8 -r 24 -f gif - | gifsicle --optimize=3 --delay=3 > $out
end
