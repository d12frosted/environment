function convert_to_gif -a file scale -d "Convert video to gif"
  if test ! -f $file
    echo "You must provide input video file as first argument"
    return 1
  end

  # process input
  set -l out (echo $file | sed 's/\(.*\)\..*/\1.gif/')

  # make sure that everything in installed
  __package_install ffmpeg
  __package_install gifsicle

  if test -z $scale
    set scale 1
  end

  # just do the work
  ffmpeg -i $file -vf "scale=iw*$scale:ih*$scale" -pix_fmt rgb8 -r 24 -f gif - | gifsicle --optimize=3 --delay=3 > $out
end
