local cmd_alt = {"cmd", "alt"}
local cmd_alt_ctrl = {"cmd", "alt", "ctrl"}
local cmd_ctrl = {"cmd", "ctrl"}
local main_monitor = "DELL U3415W"
local second_monitor = "Color LCD"


local emacs = "emacs"
local term = "iterm2"

hs.window.animationDuration = 0.2

-- layouts

-- > hs.window.get("Zulip"):frame()
-- hs.geometry.rect(4.0,542.0,1352.0,898.0)
chatStuff = {
  {"Zulip", nil, main_monitor, nil, nil, hs.geometry.rect(4,542,1352,898)},
  {"Slack", nil, main_monitor, nil, nil, hs.geometry.rect(1357,539,1352,898)}
}

function hs.screen.get(screen_name)
  local allScreens = hs.screen.allScreens()
  for i, screen in ipairs(allScreens) do
    if screen:name() == screen_name then
      return screen
    end
  end
end

function hs.screen.minWidth(isFullscreen)
  local min_width = math.maxinteger
  local screen = hs.screen.mainScreen()
  local screen_frame = screen:frame()
  if (isFullScreen) then
    screen_frame = screen:fullFrame()
  end
  min_width = math.min(min_width, screen_frame.w)
  return min_width
end

function hs.screen.minHeight(isFullscreen)
  local min_height = math.maxinteger
  local screen = hs.screen.mainScreen()
  local screen_frame = screen:frame()
  if (isFullScreen) then
    screen_frame = screen:fullFrame()
  end
  min_height = math.min(min_height, screen_frame.h)
  return min_height
end

function hs.screen.minX(refScreen)
  local min_x = refScreen:frame().x
  return min_x
end

function hs.screen.minY(refScreen)
  local min_y = refScreen:frame().y
  return min_y
end

function hs.screen.almostMinX(refScreen)
  local min_x = refScreen:frame().x
  return min_x
end

function hs.screen.almostMinY(refScreen)
  local min_y = refScreen:frame().y
 -- end
  return min_y
end

function hs.screen.minFrame(refScreen, isFullscreen)
  return {
    x = hs.screen.minX(refScreen),
    y = hs.screen.minY(refScreen),
    w = hs.screen.minWidth(isFullscreen),
    h = hs.screen.minHeight(isFullscreen)
  }
end

-- +-----------------+
-- |        |        |
-- |        |  HERE  |
-- |        |        |
-- +-----------------+
function hs.window.right(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  minFrame.x = minFrame.x + (minFrame.w/2)
  minFrame.w = minFrame.w/2
  win:setFrame(minFrame)
end

-- +-----------------+
-- |        |        |
-- |  HERE  |        |
-- |        |        |
-- +-----------------+
function hs.window.left(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  minFrame.w = minFrame.w/2
  win:setFrame(minFrame)
end

-- +-----------------+
-- |      HERE       |
-- +-----------------+
-- |                 |
-- +-----------------+
function hs.window.up(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  minFrame.h = minFrame.h/2
  win:setFrame(minFrame)
end

-- +-----------------+
-- |                 |
-- +-----------------+
-- |      HERE       |
-- +-----------------+
function hs.window.down(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  minFrame.y = minFrame.y + minFrame.h/2
  minFrame.h = minFrame.h/2
  win:setFrame(minFrame)
end

-- +-----------------+
-- |  HERE  |        |
-- +--------+        |
-- |                 |
-- +-----------------+
function hs.window.upLeft(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  minFrame.w = minFrame.w/2
  minFrame.h = minFrame.h/2
  win:setFrame(minFrame)
end

-- +-----------------+
-- |                 |
-- +--------+        |
-- |  HERE  |        |
-- +-----------------+
function hs.window.downLeft(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  win:setFrame({
    x = minFrame.x,
    y = minFrame.y + minFrame.h/2,
    w = minFrame.w/2,
    h = minFrame.h/2
  })
end

-- +-----------------+
-- |                 |
-- |        +--------|
-- |        |  HERE  |
-- +-----------------+
function hs.window.downRight(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  win:setFrame({
    x = minFrame.x + minFrame.w/2,
    y = minFrame.y + minFrame.h/2,
    w = minFrame.w/2,
    h = minFrame.h/2
  })
end

-- +-----------------+
-- |        |  HERE  |
-- |        +--------|
-- |                 |
-- +-----------------+
function hs.window.upRight(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  win:setFrame({
    x = minFrame.x + minFrame.w/2,
    y = minFrame.y,
    w = minFrame.w/2,
    h = minFrame.h/2
  })
end

-- +------------------+
-- |                  |
-- |    +--------+    +--> minY
-- |    |  HERE  |    |
-- |    +--------+    |
-- |                  |
-- +------------------+
-- Where the window's size is equal to
-- the smaller available screen size
function hs.window.fullscreenCenter(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  win:setFrame(minFrame)
end

-- +------------------+
-- |                  |
-- |  +------------+  +--> minY
-- |  |    HERE    |  |
-- |  +------------+  |
-- |                  |
-- +------------------+
function hs.window.fullscreenAlmostCenter(win)
  local offsetW = hs.screen.minX(win:screen()) - hs.screen.almostMinX(win:screen())
  win:setFrame({
    x = hs.screen.almostMinX(win:screen()),
    y = hs.screen.minY(win:screen()),
    w = hs.screen.minWidth(isFullscreen) + (2 * offsetW),
    h = hs.screen.minHeight(isFullscreen)
  })
end

-- It like fullscreen but with minY and minHeight values
-- +------------------+
-- |                  |
-- +------------------+--> minY
-- |       HERE       |
-- +------------------+--> minHeight
-- |                  |
-- +------------------+
function hs.window.fullscreenWidth(win)
  local minFrame = hs.screen.minFrame(win:screen(), false)
  win:setFrame({
    x = win:screen():frame().x,
    y = minFrame.y,
    w = win:screen():frame().w,
    h = minFrame.h
  })
end


-- set to small size i like
-- 482.0,189.0,1565.0,1157.0
function hs.window.browserSize(win)
  win:setFrame({
      x = 482.0,
      y = 189.0,
      w = 1565.0,
      h = 1157.0
  })
end

function hs.window.slackSize(win)
  win:setFrame({
      x = 304.0,
      y = 469.0,
      w = 1352.0,
      h = 898.0
  })
end

function hs.window.nudgeLeft(win)
  local newX = win:frame().x - 10
  win:move({newX, win:frame().y, win:frame().w, win:frame().h})
end

function hs.window.nudgeRight(win)
  local newX = win:frame().x + 10
  win:move({newX, win:frame().y, win:frame().w, win:frame().h})
end

function hs.window.nudgeUp(win)
  local newY = win:frame().y - 10
  win:move({win:frame().x, newY, win:frame().w, win:frame().h})
end

function hs.window.nudgeDown(win)
  local newY = win:frame().y + 10
  win:move({win:frame().x, newY, win:frame().w, win:frame().h})
end

local mNone = {}

-- Modal activation / deactivation
local keys = {}
local modalActive = false


function bind( mods, key, callback )
  table.insert( keys, hs.hotkey.new( mods, key, callback ) )
end

function disableKeys()
  modalActive = false
  for keyCount = 1, #keys do
    keys[ keyCount ]:disable()
  end
  hs.alert.closeAll()
end

function enableKeys()
  modalActive = true
  for keyCount = 1, #keys do
    keys[ keyCount ]:enable()
  end
  hs.alert.show( "DOIN WORK", 999999 )
end

hs.hotkey.bind( {"ctrl"}, "'", function()
    if( modalActive ) then
      disableKeys()
    else
      enableKeys()
    end
end )


bind( mNone, 'h', function()
        local win = hs.window.focusedWindow()
        win:left()
        disableKeys()
end )

bind( mNone, 'l', function()
        local win = hs.window.focusedWindow()
        win:right()
        disableKeys()
end)

bind( mNone, 'space', function()
        local win = hs.window.focusedWindow()
        win:maximize()
        disableKeys()
end)

bind( {"ctrl"}, 'h', function()
    local win = hs.window.focusedWindow()
    win:nudgeLeft()
end)

bind( {"ctrl"}, 'l', function()
    local win = hs.window.focusedWindow()
    win:nudgeRight()
end)

bind( {"ctrl"}, 'j', function()
    local win = hs.window.focusedWindow()
    win:nudgeDown()
end)

bind( {"ctrl"}, 'k', function()
    local win = hs.window.focusedWindow()
    win:nudgeUp()
end)

bind( mNone, 'b', function()
        local win = hs.window.focusedWindow()
        win:browserSize()
        disableKeys()
end)

bind( mNone, 's', function()
        local win = hs.window.focusedWindow()
        win:slackSize()
        disableKeys()
end)

bind( mNone, 'e', function()
        hs.application.open(emacs)
        disableKeys()
end)

bind( mNone, 't', function()
        local t = hs.application.find(term):activate()
        hs.alert.show(hs.application.find(term):focusedWindow():title(), 5)
        disableKeys()
end)

bind( mNone, 'v', function()
        local em = hs.application.find(emacs):focusedWindow()
        local te = hs.application.find(term):focusedWindow()
        em:left()
        te:right()
        disableKeys()
end)

bind( mNone, 'c', function()
        hs.layout.apply(chatStuff)
        disableKeys()
end)

bind( mNone, 'ESCAPE', function()
        disableKeys()
end )

bind( mNone, 'RETURN', function()
        disableKeys()
end )

bind( cmd_ctrl, 'r', function()
        hs.reload()
        disableKeys()
end)
