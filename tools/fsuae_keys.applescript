on run argv
    tell application "System Events"
        tell process "fs-uae" to set frontmost to true
        if item 1 of argv is "focus" then return
        if item 1 of argv is "screenshot" then
            keystroke "s" using command down
        else if item 1 of argv is "start" then
            keystroke space
        else if item 1 of argv is "smoke" then
            keystroke space
            delay 3.5
            keystroke "s" using command down
            delay 2.5
            keystroke "s" using command down
        else if item 1 of argv is "quit" then
            keystroke "q" using command down
        end if
    end tell
end run
