import face
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

makeLibrary(filepath:"planeG.so")
let cmdpipe = pipeInit("helo", "ok")
callInit(cmdWake, cmdpipe)
