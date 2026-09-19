// Sustain 45 seconds of assisted gameplay in an explicitly CHEAT_MODE=1 trial.
import Foundation
import CoreGraphics
import AppKit
func key(_ code: CGKeyCode, _ down: Bool) {
    CGEvent(keyboardEventSource: nil, virtualKey: code, keyDown: down)!.post(tap: .cghidEventTap)
}
func screenshot() {
    let p=Process()
    p.executableURL=URL(fileURLWithPath:"/usr/bin/osascript")
    p.arguments=["tools/fsuae_keys.applescript","screenshot"]
    try! p.run(); p.waitUntilExit()
}
guard CGPreflightPostEventAccess() else { fatalError("Event-posting permission required") }
guard let app=NSWorkspace.shared.runningApplications.first(where: { $0.executableURL?.lastPathComponent == "fs-uae" }) else { fatalError("FS-UAE is not running") }
app.activate(options:[.activateAllWindows])
Thread.sleep(forTimeInterval:0.5)
key(28,true) // hold top-row 8 before starting so assistance is active immediately
key(49,true)
Thread.sleep(forTimeInterval:0.08)
key(49,false)
for _ in 0..<3 {
    Thread.sleep(forTimeInterval:15)
    screenshot()
}
key(28,false)
