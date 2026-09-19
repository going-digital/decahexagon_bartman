// Host-only macOS smoke test. Compile with swiftc, focus FS-UAE first.
import Foundation
import CoreGraphics

func key(_ code: CGKeyCode, _ down: Bool, _ flags: CGEventFlags = []) {
    let event = CGEvent(keyboardEventSource: nil, virtualKey: code, keyDown: down)!
    event.flags = flags
    event.post(tap: .cghidEventTap)
}
func screenshot() {
    // SDL needs actual modifier events, not just flags on the S event.
    key(55, true, .maskCommand)
    key(1, true, .maskCommand)
    key(1, false, .maskCommand)
    key(55, false)
}
guard CGPreflightPostEventAccess() else {
    fatalError("macOS event-posting permission is required for the smoke test")
}
key(49, true); key(49, false) // Space: title/game-over -> new run
Thread.sleep(forTimeInterval: 2)
key(28, true) // ANSI top-row 8
Thread.sleep(forTimeInterval: 10)
screenshot()
Thread.sleep(forTimeInterval: 10)
key(28, false)
Thread.sleep(forTimeInterval: 6)
screenshot()
