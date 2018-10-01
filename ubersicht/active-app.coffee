command: "sh ./scripts/getActiveApp.sh"

refreshFrequency: 1000

render: (output) ->
  """
    <link rel="stylesheet" type="text/css" href="./assets/default.css">
    <link rel="stylesheet" type="text/css" href="./assets/colors.css">
    <div class="activeApp white"></div>
  """

style: """
  width: 100%
  position: absolute
  margin: 0 auto
  margin-top: 2px
  text-align: center
  font: 14px "HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif
  font-weight: 700
"""

update: (output, domEl) ->
  $(domEl).find('.activeApp').html(output);
