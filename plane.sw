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

var argc = 0;
var prtc = 0;
let optc = 10;
for _ in CommandLine.arguments {argc = argc + 1}
if (argc < optc) {prtc = optc} else {prtc = argc}
utilAlloc(Int32(argc),Int32(prtc),Int32(optc));
argc = 0; for arg in CommandLine.arguments {utilArg(arg,Int32(argc)); argc = argc + 1}
utilOpt("io",Int32(0))

makeLibrary(filepath:"planeG.so")
let cmdpipe = pipeInit("helo", "ok")
callInit(cmdWake, cmdpipe)
