commands =
  focus: "echo $(/usr/local/bin/chunkc tiling::query --window name)"
  appName: "/usr/bin/osascript -e 'tell application \"System Events\"' -e 'set frontApp to name of first application process whose frontmost is true' -e 'end tell'"

command: "echo " +
         "$(#{commands.appName}):::"

refreshFrequency: '10s'

render: () ->
  """
     <div class="container" id="window">
      <div class="widg" id="window">
          <div class="icon-container" id="music-icon-container">
          </div>
        <span class="output" id="window-output"></span>
      </div>
    </div>
  """

update: ( output, domEl ) ->
  console.log(output)
  window = output.split( /:::/g )[ 0 ]
  $("#window-output").text(window)
