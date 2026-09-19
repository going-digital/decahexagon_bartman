// Host-only macOS menu smoke test. Run from the repository root.
// Compile: swiftc tools/fsuae_menu.swift -o out/fsuae_menu
import Foundation
import CoreGraphics
import AppKit

func key(_ code: CGKeyCode, _ down: Bool, _ flags: CGEventFlags = []) {
    let event = CGEvent(keyboardEventSource: nil, virtualKey: code, keyDown: down)!
    event.flags = flags
    event.post(tap: .cghidEventTap)
}
func screenshot() {
    let process=Process()
    process.executableURL=URL(fileURLWithPath:"/usr/bin/osascript")
    process.arguments=["tools/fsuae_keys.applescript","screenshot"]
    try! process.run()
    process.waitUntilExit()
}
guard CGPreflightPostEventAccess() else {
    fatalError("macOS event-posting permission is required for the smoke test")
}
let codes: [String: CGKeyCode] = ["left":123,"right":124,"start":49,"back":53,"retry":49]
guard CommandLine.arguments.count == 2, let code=codes[CommandLine.arguments[1]] else {
    fatalError("usage: fsuae_menu left|right|start|back|retry")
}
guard let app=NSWorkspace.shared.runningApplications.first(where: { $0.executableURL?.lastPathComponent == "fs-uae" }) else { fatalError("FS-UAE is not running") }
app.activate(options:[.activateAllWindows])
Thread.sleep(forTimeInterval:0.5)
key(code,true)
Thread.sleep(forTimeInterval:0.08) // spans hardware polls; one menu move
if CommandLine.arguments[1] == "retry" {
    Thread.sleep(forTimeInterval:0.4)
    screenshot()
    Thread.sleep(forTimeInterval:5)
    screenshot()
}
key(code,false)
Thread.sleep(forTimeInterval:0.4)
screenshot()
Thread.sleep(forTimeInterval:2)
screenshot()
