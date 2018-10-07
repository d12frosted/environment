commands =
  activeSpace: "echo $(/usr/local/bin/chunkc tiling::query -d id)"

command: "echo " +
         "$(#{commands.activeSpace}):::"

refreshFrequency: '10s'

icons:
  "1" : "far fa-code"
  "2" : "fab fa-firefox"
  "3" : "fas fa-comments"
  "4" : "fas fa-film"

render: ( ) ->
  """
    <div class="container pinned" id="desktop">
      <div class="widg " id="home">
        <div class="icon-container" id="home-icon-container">
         <i class="far fa-home" id="home-current-icon"></i>
        </div>
        <span class="output" id="desktop-output">1</span>
      </div>

    </div>
  """

currentValue: 0

update: ( output, domEl ) ->
  values = []
  values.desktop = output.split( /:::/g )[ 0 ]

  controls = ['desktop']
  for control in controls
    icon = @icons[values.desktop]
    outputId = "#"+control+"-output"
    updatedValue = values[control]

    if updatedValue != @currentValue
      @currentValue = updatedValue
      $("#{outputId}").text("#{ updatedValue }")
      $("#home-current-icon").removeClass()
      $("#home-current-icon").addClass(icon)

  #$(domEl).find("#desktop-output").text("#{activeSpace}")
  #$(domEl).on 'click', "#home-icon-container", (e) -> #switch to desktop 1??


  #$(domEl).find(".active").removeClass("active")
  #$(domEl).find("#desk"+activeSpace).addClass('active')
