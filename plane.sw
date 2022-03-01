import util
import type
import plane
import AppKit
import Metal

func makeLibrary(filepath: String)
{
	print(filepath)
}
func cmdWake(_ sub: CInt)
{
	let cmd = readFile(sub)
	print("cmdWake \(cmd.act!)")
}

// MAIN

let lstv = ["rwf-","rwf","rw","rwf",""]
let fncv = [utilUsage,utilRaw,utilEnv,utilPipe,utilFile]
let glbv = ["","","",""]
var argc = 0
var lstc = 0
var glbc = 0
for _ in CommandLine.arguments {argc = argc + 1}
for _ in lstv {lstc = lstc + 1}
for _ in glbv {glbc = glbc + 1}
utilAlloc(Int32(lstc),Int32(argc),Int32(glbc));
glbc = 0; for glb in glbv {utilGlbv(Int32(glbc),utilUnionS(glb)); glbc = glbc + 1}
argc = 0; for arg in CommandLine.arguments {utilArgv(Int32(argc),Int32(0),utilUnionS(arg)); argc = argc + 1}
lstc = 0; for (lst,fnc) in zip(lstv,fncv) {fnc(Int32(lstc),lst); lstc = lstc + 1}
makeLibrary(filepath:"planeG.so")
let lst = utilGlob(SUB).i;
let opt = utilStrstr(lst,lst,FCH);
var arg:Int32 = 0; while (utilTest(lst,arg,opt,NXT,EQU) != 0) {callInit(cmdWake,utilHash(lst,arg,opt,NXT,EQU).i); arg = arg + 1}
