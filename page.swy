import AppKit
var file:NSScrollView!
var text:NSTextView!
var view:NSView!
var window:NSWindow!
var char:NSRange!
var field:NSRange!
var fields:[NSRange] = []
var saved:[String] = []
func setFrame()
{
	file.frame = view.frame
}
func isValid(_ field: NSRange) -> Bool
{
	return false // TODO return whether field is validly parenthesized
}
func inField(_ field: NSRange, _ index: Int) -> Bool
{
	return (index >= field.location && index < field.location+field.length)
}
func ofFields(_ fields: [NSRange], _ index: Int) -> NSRange?
{
	for field in fields {
		if inField(field,index) {return field}
	}
	return nil
}
func noFields(_ fields: [NSRange], _ index: Int) -> [NSRange]
{
	var result:[NSRange] = []
	for field in fields {
		if !inField(field,index) {result.append(field)}
	}
	return result
}
func toField(_ index: Int) -> NSRange
{
	return NSRange(location:index,length:5) // expand range with nesting
}
class getDelegate : NSObject, NSWindowDelegate
{
	func windowShouldClose(_ sender: NSWindow) -> Bool
	{
		NSApp.stop(nil)
		return true
	}
	func windowDidResize(_ notification: Notification)
	{
		setFrame()
	}
}
class getView : NSView
{
	override func hitTest(_ point: NSPoint) -> NSView?
	{
		var result = super.hitTest(point)
		if (result == text)
		{
			result = self
		}
		return result
	}
	override func mouseDown(with event: NSEvent)
	{
		var textLocation = NSEvent.mouseLocation
		let textOrigin = window!.frame.origin
		textLocation.x = textLocation.x-textOrigin.x
		textLocation.y = textLocation.y-textOrigin.y
		textLocation.y = file.documentVisibleRect.size.height+file.documentVisibleRect.origin.y-textLocation.y
		let index = text.layoutManager!.characterIndex(for:textLocation,in:text.textContainer!,fractionOfDistanceBetweenInsertionPoints:nil)
		if (char != nil)
		{
			text.textStorage!.removeAttribute(.foregroundColor,range:char)
		}
		char = NSRange(location:index,length:1)
		text.textStorage!.addAttribute(.foregroundColor,value:NSColor.red.withAlphaComponent(0.25),range:char!)
		let initial = (field == nil)
		let outside = (!initial && !inField(field!,index))
		let nofield = (initial || outside)
		let invalid = (!initial && !isValid(field!))
		let former = ofFields(fields,index)
		let nofound = (former == nil)
		if (outside)
		{
			text.textStorage!.removeAttribute(.backgroundColor,range:field)
		}
		if (outside && !invalid)
		{
			// TODO write text at field to pipe
		}
		if (outside && !invalid && !nofound)
		{
			fields = noFields(fields,index)
		}
		if (outside && invalid && nofound)
		{
			fields.append(field)
		}
		if (outside && invalid)
		{
			text.textStorage!.addAttribute(.backgroundColor,value:NSColor.yellow.withAlphaComponent(0.25),range:field!)
		}
		if (nofield && !nofound)
		{
			field = former
			text.textStorage!.removeAttribute(.backgroundColor,range:field)
		}
		if (nofield && nofound)
		{
			field = toField(index)
			// TODO get field from text to saved
		}
		text.textStorage!.addAttribute(.backgroundColor,value:NSColor.blue.withAlphaComponent(0.25),range:field!)
	}	
}
// MAIN
	let rect = NSMakeRect(
		CGFloat(0), CGFloat(0),
		CGFloat(1024), CGFloat(512))
	var lines = 0
	var string = ""
	while (lines < 20) {
		string = string+"comment Str(hello) comment Str(ok) comment Str(again) \(lines)\n"
		lines += 1
	}

	file = NSTextView.scrollableTextView()
	text = file.documentView as? NSTextView
	let font = NSFont.userFont(ofSize:36.0)
	text.font = font
	text.string = string
	text.backgroundColor = NSColor.white
	text.textColor = NSColor.black
	text.isEditable = false

	view = getView(frame:rect)
	view.addSubview(file)

	let mask:NSWindow.StyleMask = [.titled,.closable,.miniaturizable,.resizable]
	window = NSWindow(contentRect: rect, styleMask: mask, backing: .buffered, defer: false)
	window.title = "page"
	window.contentView = view
	let delegate = getDelegate()
	window.delegate = delegate
	window.makeKeyAndOrderFront(nil)
	setFrame()

	NSEvent.addLocalMonitorForEvents(matching:NSEvent.EventTypeMask.keyDown, handler: {(event: NSEvent) in
		print("keyPressed \(event.characters!)"); return event})
	NSApp.setActivationPolicy(NSApplication.ActivationPolicy.accessory)
	NSApp.activate(ignoringOtherApps:true)
	NSApp.run()
