commands =
  isRunning: "osascript -e 'if application \"iTunes\" is running then return true'"
  isPlaying: "osascript -e 'if application \"iTunes\" is running then tell application \"iTunes\" to if player state is playing then return true'"
  iTunes: "osascript -e 'if application \"iTunes\" is running then tell application \"iTunes\" to if player state is playing then artist of current track & \" - \" & name of current track'"

command: "echo " +
         "$(#{ commands.isRunning}):::" +
         "$(#{ commands.isPlaying}):::" +
         "$(#{ commands.iTunes}):::"

refreshFrequency: '10s'

render: ( ) ->
  """
    <div class="container">
        <div class="widg" id="play">
          <div class="icon-container" id="play-icon-container">
            <i class="fas fa-music""></i>
          </div>
          <div class="icon-container" id="prev-icon-container">
            <i class="fas fa-step-backward" id="prev-button"></i>
          </div>
          <div class="icon-container" id="play-icon-container">
            <i class="fas fa-play" id="play-button"></i>
          </div>
          <div class="icon-container" id="next-icon-container">
            <i class="fas fa-step-forward" id="next-button"></i>
          </div>
          <span class="static-output" id='play-output'></span>
        </div>

    </div>
  """

update: ( output, domEl ) ->
  output = output.split( /:::/g )
  isRunning = output[0]
  isPlaying = output[1]
  iTunes = output[2]

  isRunningChanged = @cache("isRunning", isRunning)
  isPlayingChanged = @cache("isPlaying", isPlaying)
  iTunesChanged = @cache("iTunes", iTunes)

  if isRunningChanged or isPlayingChanged or iTunesChanged
    if isRunning
      if isPlaying
        @handlePlayIcon(domEl, true)
        @handlePlayIcon(domEl, true)
        $(domEl).find('#play-output').text(iTunes)
      else
        @handlePlayIcon(domEl, false)
        $("#play-output").text("Paused")
    else
      $("#play-output").text("")

#
# ─── CACHE ─────────────────────────────────────────────────────────
#

cache_storage: {}

cache: (key, value) ->
  if @cache_storage and @cache_storage[key] == value
    return false
  else
    @cache_storage[key] = value
    return true

#
# ─── HANDLES  ─────────────────────────────────────────────────────────
#

handlePlay: (domEl, status) ->
  @run "osascript -e 'tell application \"iTunes\" to playpause'"
  @handlePlayIcon(domEl, status)
  @refresh()

handlePlayIcon: (domEl, status) ->
  if status == 'NULL'
    if $(domEl).find('#play-button').hasClass('fa-play')
      status = true
    else
      status = false

  if status == true
    $(domEl).find('#play-button').removeClass()
    $(domEl).find('#play-button').addClass('fas fa-pause')
    $(domEl).find('#play').addClass('open')
  else
    $(domEl).find('#play-button').removeClass()
    $(domEl).find('#play-button').addClass('fas fa-play')
    $(domEl).find('#play').removeClass('open')

handlePrev: (domEl) ->
  @run "osascript -e 'tell application \"iTunes\" to previous track'"
  $(domEl).find('#play-button').removeClass()
  $(domEl).find('#play-button').addClass('fas fa-pause')
  @refresh()

handleNext: (domEl) ->
  @run "osascript -e 'tell application \"iTunes\" to next track'"
  $(domEl).find('#play-button').removeClass()
  $(domEl).find('#play-button').addClass('fas fa-pause')
  @refresh()

afterRender: (domEl) ->
  $(domEl).on 'click', '#home', => @run "open ~/"

  $(domEl).on 'click', '#prev-button', => @handlePrev(domEl)
  $(domEl).on 'click', '#play-button', => @handlePlay(domEl, 'NULL')
  $(domEl).on 'click', '#next-button', => @handleNext(domEl)

#
# ─── ANIMATION  ─────────────────────────────────────────────────────────
#
  # ---- OPEN
  $(domEl).on 'mouseover', ".widg", (e) => $(domEl).find( $($(e.target))).addClass('open')
  $(domEl).on 'mouseover', ".icon-container", (e) => $(domEl).find( $($(e.target))).parent().addClass('open')
  $(domEl).on 'mouseover', ".output", (e) => $(domEl).find( $($(e.target))).parent().addClass('open')

  $(domEl).on 'mouseout', ".widg", (e) => $(domEl).find( $($(e.target))).removeClass('open')
  $(domEl).on 'mouseout', ".icon-container", (e) => $(domEl).find( $($(e.target))).parent().removeClass('open')
  $(domEl).on 'mouseout', ".output", (e) => $(domEl).find( $($(e.target))).parent().removeClass('open')

  #$(domEl).on 'click', ".widg", (e) => @toggleOption( domEl, e, 'pinned')
#
# ─── CLICKS  ─────────────────────────────────────────────────────────
#

toggleOption: (domEl, e, option) ->
  target = $(domEl).find( $($(e.target))).parent()

  if target.hasClass("#{ option }")
    $(target).removeClass("#{ option }")
    $(output).removeClass("#{ option }")
  else
    $(target).addClass("#{ option }")
    $(output).addClass("#{ option }")
