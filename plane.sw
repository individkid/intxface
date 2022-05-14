import argx
import share
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

// TODO initialize window and graphics and internal
shareInit()
// TODO change the factory to call cmdWake and such
for arg in CommandLine.arguments {useArgument(arg)}
makeLibrary(filepath:"planeG.so")
runProgram()
