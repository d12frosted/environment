package.path = package.path .. ";./lib/?.lua"

require('pl.stringx').import()

--------------------------------------------------------------------------------
-- * Globals

d12 = {}

function reg(key, val)
  d12[key] = val
end

function d12.trim(str)
  return string.match(str, "^%s*(.*%S)") or ""
end

function d12.remove_trailing_newline(str)
  return string.match(str, ".*%S") or ""
end

function d12.unsurround(str, a, b)
  return string.gsub(str, string.format("%s(.*)%s", a, b or a), "%1") or ""
end

--------------------------------------------------------------------------------
-- * Emacs Lisp Helpers

local elisp = {}
reg("elisp", elisp)

function elisp.execute(format, ...)
  local command = string.format("emacsclient " .. " -e '" .. elisp.escape(format) .. "'", ...)
  local result, status = hs.execute(command, true)
  result = d12.remove_trailing_newline(result)
  if not status then
    error(result)
  end
  return result, status
end

function elisp.escape(command)
  return string.gsub(command, "\"", "\\\"")
end

function elisp.is_true(val)
  if val == "nil" then
    return false
  end
  return true
end

function elisp.extract(val, def)
  if val == "nil" then
    return def
  end
  return val
end

--------------------------------------------------------------------------------
-- * Current task

do
  local titlelength = 32
  local orgtask = hs.menubar.new()
  local updateTimer
  local isIconSet = false

  function isActiveClock()
    return elisp.is_true(elisp.execute "(d12-org/clock-active-p)")
  end

  function setIcon()
    if isIconSet then
      return
    end
    local home = string.gsub(hs.execute "echo $HOME", "\n", "")
    local path = string.format("%s/.environment/hammerspoon/img/org-mode-icon.png", home)
    orgtask:setIcon (path)
    isIconSet = true
  end

  function unsafeUpdate()
    setIcon()
    if isActiveClock() then
      local title = elisp.extract(elisp.execute "(d12-org/clock-get-decription)", "Inactive")
      local name = elisp.execute "org-clock-heading"
      local category = elisp.extract(elisp.execute "(d12-org/clock-get-property \"CATEGORY\")", "Unknown")
      local priority = elisp.extract(elisp.execute "(d12-org/clock-get-property \"PRIORITY\")", "Unknown")
      local state = elisp.extract(elisp.execute "(d12-org/clock-get-property \"TODO\")", "Unknown")
      local total = elisp.extract(elisp.execute "(d12-org/clock-get-total-time-string)", "0:00")
      local current = elisp.extract(elisp.execute "(d12-org/clock-get-current-time-string)", "0:00")

      orgtask:setTitle(string.shorten(d12.unsurround(title, "\""), titlelength))
      orgtask:setMenu
      {
        { title = "Name: " .. d12.unsurround(name, "\"") },
        { title = "Category: " .. d12.unsurround(category, "\"") },
        { title = "State: " .. d12.unsurround(state, "\"") },
        { title = "Priority: #" .. d12.unsurround(priority, "\"") },
        { title = "Total: " .. d12.unsurround(total, "\"") },
        { title = "Current: " .. d12.unsurround(current, "\"") },
      }
    else
      orgtask:setTitle "Inactive"
      orgtask:setMenu { }
    end
  end

  function updateMsgHander(msg)
    print("--------------------------------")
    print("Unexpected error occured.")
    print(msg)
    print(debug.traceback())
    print("--------------------------------")
    orgtask:setTitle "[Error]"
    orgtask:setMenu
    {
      { title = "Error: " .. d12.unsurround(msg, "\"") },
    }
  end

  function update()
    xpcall(unsafeUpdate, updateMsgHander)
  end

  if orgtask then
    update()
    local timer = hs.timer.new(5, update, true)
    reg("orgtask_timer", timer)
    timer:start()
  end
end
