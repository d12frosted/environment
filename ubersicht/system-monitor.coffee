
commands =
  cpu : "ps -A -o %cpu | awk '{s+=$1} END {printf(\"%.2f\",s/8);}'"
  mem : "ps -A -o %mem | awk '{s+=$1} END {print s \"%\"}' "
  hdd : "df -H | grep -m 1 /disk1 | awk -F\" \" '{print $4}'"


command: "echo " +
         "$(#{ commands.cpu }):::" +
         "$(#{ commands.mem }):::" +
         "$(#{ commands.hdd }):::"

refreshFrequency: false

render: ( ) ->
  """
    <div class="container">

          <div class="widg" id="cpu">
          <div class="icon-container" id='cpu-icon-container'>
            <i class="fa fa-spinner"></i>
          </div>
            <span class="output" id="cpu-output"></span>
          </div>

          <div class="widg" id="mem">
          <div class="icon-container" id='mem-icon-container'>
            <i class="fas fa-server"></i>
            </div>
            <span class="output" id="mem-output"></span>
          </div>

          <div class="widg" id="hdd">
          <div class="icon-container" id='hdd-icon-container'>
            <i class="fas fa-hdd"></i>
            </div>
              <span class="output" id="hdd-output"></span>
          </div>


    </div>
  """

update: ( output, domEl ) ->
  output = output.split( /:::/g )

  cpu = output[ 0 ]
  mem = output[ 1 ]
  hdd = output[ 2 ]

  $( "#cpu-output").text("#{ cpu }")
  $( "#mem-output").text("#{ mem }")
  $( "#hdd-output").text("#{ hdd }")

  @handleSysmon( domEl, Number( cpu ), '#cpu' )
  @handleSysmon( domEl, Number( mem.replace( /%/g, "") ), '#mem' )
  @handleSysmon( domEl, Number( hdd.replace( /%/g, "") ), '#hdd' )


#
# ─── HANDLE SYSMON –─────────────────────────────────────────────────────────
#
handleSysmon: ( domEl, sysmon, monid ) ->
  div = $(domEl)

  div.find(monid).removeClass('blue')
  div.find(monid).removeClass('cyan')
  div.find(monid).removeClass('green')
  div.find(monid).removeClass('yellow')
  div.find(monid).removeClass('magenta')
  div.find(monid).removeClass('red')

  if sysmon <= 10
    div.find(monid).addClass('blue')
  else if sysmon <= 20
    div.find(monid).addClass('blue')
  else if sysmon <= 40
    div.find(monid).addClass('cyan')
  else if sysmon <= 50
    div.find(monid).addClass('green')
  else if sysmon <= 75
    div.find(monid).addClass('yellow')
  else
    div.find(monid).addClass('red')

#
# ─── UNIVERSAL CLICK AND ANIMATION HANDLING  ─────────────────────────────────────────────────────────
#
# afterRender: (domEl) ->
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
