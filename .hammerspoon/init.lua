-- Reload Hammerspoon config
hs.hotkey.bind({ "cmd", "ctrl" }, "R", function ()
    hs.reload()
    hs.notify.new({
        title = "Hammerspoon Config Reloaded",
        autoWithdraw = true,
        withdrawAfter = 2,
    }):send()
end)

-- Maximize current window
hs.hotkey.bind({ "cmd", "ctrl" }, "return", function ()
    local focusedWindow = hs.window.focusedWindow()
    focusedWindow:maximize(0)
end)

-- size focused window to left half of display
hs.hotkey.bind({"cmd", "ctrl"}, "a", function ()
    local win = hs.window.focusedWindow()
    local frame = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    frame.x = max.x
    frame.y = max.y
    frame.w = max.w / 2
    frame.h = max.h
    win:setFrame(frame)
end)

-- size focused window to right half of display
hs.hotkey.bind({"cmd", "ctrl"}, "s", function ()
    local win = hs.window.focusedWindow()
    local frame = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    frame.x = max.x + max.w / 2
    frame.y = max.y
    frame.w = max.w / 2
    frame.h = max.h
    win:setFrame(frame)
end)

hs.hotkey.bind({ "cmd", "ctrl" }, "T", function ()
    hs.execute("/usr/local/bin/code ~/playground/todos/todo.txt")
end)

-- Open Gmail
hs.hotkey.bind({ "cmd", "ctrl" }, "M", function ()
    hs.urlevent.openURL("https://mail.google.com")
end)

-- this is workaround for the key repeat issue
-- https://stackoverflow.com/questions/40986242/key-repeats-are-delayed-in-my-hammerspoon-script
function keyStroke (key)
    local event = require("hs.eventtap").event
    event.newKeyEvent({}, string.lower(key), true):post()
    event.newKeyEvent({}, string.lower(key), false):post()
end

function bindIfNotEmacs (binding)
    local windowFrame = hs.window.filter
    local emacsWindowFrame = windowFrame.new("Emacs")
    emacsWindowFrame:subscribe(windowFrame.windowFocused, function ()
        binding:disable()
    end):subscribe(windowFrame.windowUnfocused, function()
        binding:enable()
    end)
end

-- Remap ctrl + [ to send esc key
hs.hotkey.bind({ "ctrl" }, "[",
    function () keyStroke("escape") end,
    nil,
    function () keyStroke("escape") end
)

-- Remap ctrl + g to send esc key
local escBinding = hs.hotkey.bind({ "ctrl" }, "g",
    function () keyStroke("escape") end,
    nil,
    function () keyStroke("escape") end
)
bindIfNotEmacs(escBinding)

-- Remap ctrl + p to up key
local upBinding = hs.hotkey.bind({ "ctrl" }, "p",
    function () keyStroke("up") end,
    nil,
    function () keyStroke("up") end
)
bindIfNotEmacs(upBinding)

-- Remap ctrl + n to down key
local downBinding = hs.hotkey.bind({ "ctrl" }, "n",
    function () keyStroke("down") end,
    nil,
    function () keyStroke("down") end
)
bindIfNotEmacs(downBinding)

-- Remap ctrl + f to right key
local forwardBinding = hs.hotkey.bind({ "ctrl" }, "f",
    function () keyStroke("right") end,
    nil,
    function () keyStroke("right") end
)
bindIfNotEmacs(forwardBinding)

-- Remap ctrl + b to down key
local backBinding = hs.hotkey.bind({ "ctrl" }, "b",
    function () keyStroke("left") end,
    nil,
    function () keyStroke("left") end
)
bindIfNotEmacs(backBinding)

-- Remap ctrl + v to pageup key
local pageupBinding = hs.hotkey.bind({ "ctrl", "cmd" }, ",",
    function () keyStroke("pageup") end,
    nil,
    function () keyStroke("pageup") end
)
bindIfNotEmacs(pageupBinding)

-- Remap ctrl + v to pagedown key
local pagedownBinding = hs.hotkey.bind({ "ctrl", "cmd" }, ".",
    function () keyStroke("pagedown") end,
    nil,
    function () keyStroke("pagedown") end
)
bindIfNotEmacs(pagedownBinding)
