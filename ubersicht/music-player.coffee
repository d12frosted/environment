commands =
  isRunning: """osascript -e 'if application "iTunes" is running or application "VOX" is running then return true'"""
  isPlaying: """osascript -e '
                  if application "iTunes" is running then
                    tell application "iTunes" to if player state is playing then return true
                  else if application "VOX" is running then
                    tell application "VOX" to if player state is 1 then return true
                  end if
                '
             """
  trackInfo: """osascript -e '
                  if application "iTunes" is running then
                    tell application "iTunes"
                      try
                        set whatshappening to (get player state as string)
                      end try
                      if whatshappening ≠ "paused" then
                        tell application "iTunes"
                          if exists current track then
                            set theName to the name of the current track
                            set theArtist to the artist of the current track
                            try
                              return theArtist & " - " & theName
                            on error err
                            end try
                          end if
                        end tell
                      end if
                    end tell
                  else if application "VOX" is running then
                    tell application "VOX"
                      try
                        set state to player state
                      end try
                      if player state is 1 then
                        tell application "VOX"
                          set theName to track
                          set theArtist to artist
                          try
                            return theArtist & " - " & theName
                          on error err
                          end try
                        end tell
                      end if
                    end tell
                  end if
                '
             """
  playPause: """osascript -e '
                  if application "iTunes" is running then
                    tell application "iTunes"
                      playpause
                    end tell
                  else if application "VOX" is running then
                    tell application "VOX"
                      playpause
                    end tell
                  end if
                '
             """
  prevTrack: """osascript -e '
                  if application "iTunes" is running then
                    tell application "iTunes"
                      previous track
                    end tell
                  else if application "VOX" is running then
                    tell application "VOX"
                      previous track
                    end tell
                  end if
                '
             """
  nextTrack: """osascript -e '
                  if application "iTunes" is running then
                    tell application "iTunes"
                      next track
                    end tell
                  else if application "VOX" is running then
                    tell application "VOX"
                      next track
                    end tell
                  end if
                '
             """

command: "echo " +
         "$(#{commands.isRunning}):::" +
         "$(#{commands.isPlaying}):::" +
         "$(#{commands.trackInfo}):::"

refreshFrequency: '10s'

render: () ->
  """
    <div class="widg" id="play">
      <div class="widg"">
        <div class="icon-container" id="play-icon-container">
          <i class="fas fa-music""></i>
        </div>
      </div>

      <div class="widg"">
        <div class="icon-container" id="prev-icon-container">
          <i class="fas fa-step-backward" id="prev-button"></i>
        </div>
      </div>

      <div class="widg"">
        <div class="icon-container" id="play-icon-container">
          <i class="fas fa-play" id="play-button"></i>
        </div>
      </div>

      <div class="widg"">
        <div class="icon-container" id="next-icon-container">
          <i class="fas fa-step-forward" id="next-button"></i>
        </div>
      </div>

      <span class="static-output" id='play-output'></span>
    </div>
  """

update: (output, domEl) ->
  output = output.split(/:::/g)
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
  @run commands.playPause
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
  @run commands.prevTrack
  $(domEl).find('#play-button').removeClass()
  $(domEl).find('#play-button').addClass('fas fa-pause')
  @refresh()

handleNext: (domEl) ->
  @run commands.nextTrack
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
  $(domEl).on 'mouseover', ".widg", (e) => $(domEl).find($($(e.target))).addClass('open')
  $(domEl).on 'mouseover', ".icon-container", (e) => $(domEl).find($($(e.target))).parent().addClass('open')
  $(domEl).on 'mouseover', ".output", (e) => $(domEl).find($($(e.target))).parent().addClass('open')

  $(domEl).on 'mouseout', ".widg", (e) => $(domEl).find($($(e.target))).removeClass('open')
  $(domEl).on 'mouseout', ".icon-container", (e) => $(domEl).find($($(e.target))).parent().removeClass('open')
  $(domEl).on 'mouseout', ".output", (e) => $(domEl).find($($(e.target))).parent().removeClass('open')

  #$(domEl).on 'click', ".widg", (e) => @toggleOption(domEl, e, 'pinned')
#
# ─── CLICKS  ─────────────────────────────────────────────────────────
#

toggleOption: (domEl, e, option) ->
  target = $(domEl).find($($(e.target))).parent()

  if target.hasClass("#{option}")
    $(target).removeClass("#{option}")
    $(output).removeClass("#{option}")
  else
    $(target).addClass("#{option}")
    $(output).addClass("#{option}")
