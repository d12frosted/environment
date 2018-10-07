currentLocation: "50.4501,30.5241"

setLocation: (coords) ->
  @currentLocation = "#{coords.latitude},#{coords.longitude}"

command: "echo " +
  "$(date +\"%a %d %b\")::::::"

makeDateCommand: () -> "date +\"%a %d %b\""

makeWeatherCommand: () ->
  "./scripts/getweather '#{@currentLocation}'"

makeCommand: () -> "echo " +
  "$(#{@makeDateCommand()}):::" +
  "$(#{@makeWeatherCommand()}):::"

iconMapping:
  "rain"                : "fa fa-tint"
  "snow"                : "fa fa-snowflake"
  "fog"                 : "fa fa-braille"
  "cloudy"              : "fa fa-cloud"
  "wind"                : "fa fa-align-left"
  "clear-day"           : "fas fa-sun"
  "mostly-clear-day"    : "fa fa-adjust"
  "partly-cloudy-day"   : "fa fa-cloud"
  "clear-night"         : "fa fa-star"
  "partly-cloudy-night" : "fa fa-adjust"
  "unknown"             : "fa fa-question"

refreshFrequency: '30m'

render: () ->
  """
    <div class="container">
          <div class="widg nohidden" id="date">
            <span class="output" id="date-output"></span>
          </div>
          <div class="widg open" id="weather">
            <div class="icon-container" id="weather-icon-container">
              <i class="fa fa-sun" id="weather-icon-day"></i>
            </div>
            <span class="output" id="weather-output">Loading...</span>
          </div>
    </div>
  """

update: ( output, domEl ) ->
  output = output.split( /:::/g )

  values = []

  values.date = output[ 0 ]
  values.weather = output[ 1 ]

  controls = ['date', 'weather']
  for control in controls
    outputId = "#"+control+"-output"
    currentValue = $("#{outputId}").value
    updatedValue = values[control]

    if updatedValue != currentValue
      $("#{ outputId }").text("#{ updatedValue }")

      if control is 'weather'
        if values.weather
          @handleWeather( domEl, values.weather )
        else
          $(domEl).find('#weather-output').text(String ('? °'))

#
# ─── HANDLE WEATHER ─────────────────────────────────────────────────────────
#
handleWeather: ( domEl, weatherdata ) ->
  data = JSON.parse(weatherdata)
  $(domEl).find('#weather-output').text(weatherdata)

  return unless data.currently?

  date = @getDate data.currently.time

  $(domEl).find('#weather-output').text(String (Math.round(data.currently.temperature)+'°'))
  $(domEl).find('#weather-ext-output').text(String(data.currently.summary))
  $(domEl).find("#weather-icon-day").removeClass().addClass("#{@getIcon(data.currently)}")
  $(domEl).find("#weather-icon-cloud").removeClass().addClass("#{@getIcon(data.currently)}")

  $(domEl).find("#weather").removeClass('red')
  $(domEl).find("#weather").removeClass('white')
  $(domEl).find("#weather").removeClass('cyan')
  if data.currently.temperature >= 26
    $(domEl).find('#weather').addClass('red')
    $(domEl).find('#weather-icon-container').addClass('red')
  else if data.currently.temperature >= 6
    $(domEl).find('#weather').addClass('white')
    $(domEl).find('#weather-icon-container').addClass('white')
  else
    $(domEl).find('#weather').addClass('cyan')
    $(domEl).find('#weather-icon-container').addClass('cyan')

getIcon: (data) ->
  return @iconMapping['unknown'] unless data
  return @iconMapping[data.icon]

  if data.icon.indexOf('cloudy') > -1
    if data.cloudCover < 0.25
      @iconMapping["clear-day"]
    else if data.cloudCover < 0.5
      @iconMapping["mostly-clear-day"]
    else if data.cloudCover < 0.75
      @iconMapping["partly-cloudy-day"]
    else
      @iconMapping["cloudy"]
  else
    @iconMapping[data.icon]

getDate: (utcTime) ->
  date  = new Date(0)
  date.setUTCSeconds(utcTime)
  date

#
# ─── UNIVERSAL CLICK AND ANIMATION HANDLING  ─────────────────────────────────────────────────────────
#
afterRender: (domEl) ->
  geolocation.getCurrentPosition (e) =>
    @setLocation(e.position.coords)
    @command = @makeCommand()
    @refresh()

#   $(domEl).on 'mouseover', ".widg", (e) => $(domEl).find( $($(e.target))).addClass('open')
#   $(domEl).on 'mouseover', ".icon-container", (e) => $(domEl).find( $($(e.target))).parent().addClass('open')
#   $(domEl).on 'mouseover', ".output", (e) => $(domEl).find( $($(e.target))).parent().addClass('open')

#   $(domEl).on 'mouseout', ".widg", (e) => $(domEl).find( $($(e.target))).removeClass('open')
#   $(domEl).on 'mouseout', ".icon-container", (e) => $(domEl).find( $($(e.target))).parent().removeClass('open')
#   $(domEl).on 'mouseout', ".output", (e) => $(domEl).find( $($(e.target))).parent().removeClass('open')

#   $(domEl).on 'click', ".widg", (e) => @toggleOption( domEl, e, 'pinned')

# toggleOption: (domEl, e, option) ->
#   target = $(domEl).find( $($(e.target))).parent()

#   if target.hasClass("#{ option }")
#     $(target).removeClass("#{ option }")
#     $(output).removeClass("#{ option }")
#   else
#     $(target).addClass("#{ option }")
#     $(output).addClass("#{ option }")
