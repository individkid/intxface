import plane
import AppKit
import Metal

func makeLibrary(filepath: String)
{
	print(filepath)
}
func cmdWake(_ idx: CInt)
{
	let cmd = readFile(idx)
	print("cmdWake \(cmd.num!)")
}

// MAIN

var argc = 0
var optc = 0
var lstc = 3
let opts = "iof"
for _ in CommandLine.arguments {argc = argc + 1}
for _ in opts {optc = optc + 1}
utilAlloc(Int32(argc),Int32(optc),Int32(lstc));
argc = 0; for arg in CommandLine.arguments {utilArg(arg,Int32(argc)); argc = argc + 1}
utilOpt("io",Int32(0))

makeLibrary(filepath:"planeG.so")
let cmdpipe = pipeInit("helo", "ok")
callInit(cmdWake, cmdpipe)
