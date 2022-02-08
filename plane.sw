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

let lstv = ["iof","io","f"]
let fncv = [utilRaw,utilEnv,utilPipe]
let glbv = [Int32(-1),Int32(-1)]
var argc = 0
var lstc = 0
var glbc = 0
for _ in CommandLine.arguments {argc = argc + 1}
for _ in lstv {lstc = lstc + 1}
for _ in glbv {glbc = glbc + 1}
utilAlloc(Int32(argc),Int32(lstc),Int32(glbc));
glbc = 0; for glb in glbv {utilGlbv(Int32(glbc),utilUnionI(glb)); glbc = glbc + 1}
argc = 0; for arg in CommandLine.arguments {utilArgv(Int32(argc),Int32(0),utilUnionS(arg)); argc = argc + 1}
lstc = 0; for (lst,fnc) in zip(lstv,fncv) {fnc(Int32(lstc),lst); lstc = lstc + 1}

makeLibrary(filepath:"planeG.so")
let cmdpipe = pipeInit("helo", "ok")
callInit(cmdWake, cmdpipe)
