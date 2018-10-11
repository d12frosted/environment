commands =
  allDesktops: "/usr/local/bin/chunkc tiling::query -D $(/usr/local/bin/chunkc tiling::query -m id)"
  activeDesktop: "/usr/local/bin/chunkc tiling::query -d id"

command: "echo " +
         "$(#{commands.allDesktops}):::" +
         "$(#{commands.activeDesktop}):::"

refreshFrequency: '10s'

icons:
  "1" : "far fa-code"
  "2" : "fab fa-firefox"
  "3" : "fas fa-comments"
  "4" : "fas fa-film"

defaultIcon: "fas fa-expand"

render: () ->
  """
    <div class="container" id="desktop">
      <div class="widg " id="desktop">
      </div>
    </div>
  """

update: (output, domEl) ->
  values = []
  values.desktops = output.split(/:::/g)[0].split(/ /g)
  values.desktop = output.split(/:::/g)[1]

  desktopsChanged = @cache('desktops', values.desktops)
  desktopChanged = @cache('desktop', values.desktop)

  if (desktopsChanged or desktopChanged)
    @handleDesktop(values.desktop, values.desktops)

handleDesktop: (desktop, desktops) ->
  html = ""
  for i in desktops
    html = html + @createDesktop(i, i == desktop)
  $("#desktop").html(html)

createDesktop: (desktop, active) ->
  cs = "icon-container"
  if active
    cs = cs + " active-desktop"
  """
  <div class="#{cs}" id="desktop-icon-#{desktop}-container">
    <i class="#{@getIcon(desktop)}" id="desktop-icon-#{desktop}"></i>
  </div>
  """

getIcon: (desktop) ->
  if @icons[desktop]
    @icons[desktop]
  else
    @defaultIcon

cache_storage: {}

cache: (key, value) ->
  if @cache_storage and @cache_storage[key] == value
    return false
  else
    @cache_storage[key] = value
    return true
