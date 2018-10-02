command: "./scripts/itunes info"

refreshFrequency: 10000 #ms

render: (output) ->
  """
    <div class='itunes'></div>
  """

style: """
  font: 12px Hack
  left: 14px
  bottom: 4px
  width: 850px
  height: 16px
"""

cutWhiteSpace: (text) ->
  text.replace /^\s+|\s+$/g, ""

update: (output, domEl) ->

  values = output.split(',')
  if values.length == 6
    artist = @cutWhiteSpace(values[0])
    song = @cutWhiteSpace(values[1])
    start = values[2]
    finish = values[3]
    elapsed = values[4]
    status = @cutWhiteSpace(values[5])

    if artist.length >= 30
      artist = artist.substring(0,29)
      artist = @cutWhiteSpace(artist)
      song = song + "…"

    if song.length >= 30
      song = song.substring(0,29)
      song = @cutWhiteSpace(song)
      song = song + "…"

    elapsedSeconds = elapsed
    totalSeconds = finish - start

    elapsed = elapsedSeconds / totalSeconds

    # Create mpdHtmlString
    mpdHtmlString = "<span class='icon switch'></span><span class='white'>  #{artist} - #{song}&nbsp</span>"

    emptySpace = (120 - artist.length - song.length - 3) / 2

    elapsedCounter = parseInt(elapsed * emptySpace)
    remainingCounter = emptySpace - elapsedCounter - 1

    mpdHtmlString += "<span>"
    i = 0
    while i <= elapsedCounter
      i += 1
      mpdHtmlString += " ● "

    mpdHtmlString += "</span>"
    mpdHtmlString += "<span class='grey'>"

    i = 0
    while i <= remainingCounter
      i += 1
      mpdHtmlString += " ● "

    mpdHtmlString += "</span>"


    mpdHtmlString += "<span class='sicon prev'>&nbsp&nbsp&nbsp&nbsp</span>" + " "

    if status == "playing"
       mpdHtmlString += "<span class='sicon pause'></span>" + " "
    else
       mpdHtmlString += "<span class='sicon play'></span>" + " "

    mpdHtmlString += "<span class='sicon next'>&nbsp&nbsp</span>"


    $(domEl).find('.itunes').html(mpdHtmlString)

    $(".pause").on "click", => console.log("funny") # @run "./scripts/itunes pause"
    $(".play").on "click",  => @run "./scripts/itunes play"
    $(".next").on "click",  => @run "./scripts/itunes next"
    $(".prev").on "click",  => @run "./scripts/itunes prev"
  else
    mpdHtmlString = "<span class='icon switch'></span><span class='white'>&nbsp&nbspNot playing&nbsp</span>"
    $(domEl).find('.itunes').html(mpdHtmlString)
