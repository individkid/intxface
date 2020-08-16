/*
*    help.sw
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

func noWarn<T>(_ val:T) -> T?
{
	return val
}
func noWarn<T>(_ opt:T?) -> T?
{
	return nil
}
func fromRaw<T>(_ raw:UnsafeRawPointer, _ idx:Int, _ len:Int) -> [T]
{
	return Swift.Array(0..<len).map({(sub) in raw.advanced(by:(idx+sub)*MemoryLayout<T>.size).load(as:T.self)})
}
func fromRaw<T>(_ raw:UnsafeRawPointer, _ idx:Int) -> T
{
	return raw.advanced(by:idx*MemoryLayout<T>.size).load(as:T.self)
}
func fromRaw<T>(_ raw:UnsafeRawPointer) -> T
{
	return raw.load(as:T.self)
}
func fromPtr<T>(_ ptr:UnsafePointer<T>, _ idx:Int, _ len:Int) -> [T]
{
	return Swift.Array(0..<len).map({(sub) in ptr[idx+sub]})
}
func fromPtr<T>(_ ptr:UnsafePointer<T>, _ idx:Int) -> T
{
	return ptr[idx]
}
func fromPtr<T>(_ ptr:UnsafePointer<T>) -> T
{
	return ptr.pointee
}
func fromAny<T>(_ x: Any?) -> T?
{
  return x as? T
}
func toMutable<T>(_ val:T, _ fnc:(_:UnsafeMutablePointer<T>)->Void)
{
	let ptr = UnsafeMutablePointer<T>.allocate(capacity:1); ptr[0] = val
	fnc(ptr)
	ptr.deallocate()
}
func toMutabls<T>(_ list:[T], _ fnc:(_:UnsafeMutablePointer<T>)->Void)
{
	let ptr = UnsafeMutablePointer<T>.allocate(capacity:list.count);
	for (val,idx) in zip(list,Swift.Array(0..<list.count)) {ptr[idx] = val}
	fnc(ptr)
	ptr.deallocate()
}
func toMutablee<S,T>(_ val0:S, _ val1:T, _ fnc:(_:UnsafeMutablePointer<S>,_:UnsafeMutablePointer<T>)->Void)
{
	toMutable(val0,{(ptr0:UnsafeMutablePointer<S>) in toMutable(val1,{(ptr1:UnsafeMutablePointer<T>) in fnc(ptr0,ptr1)})})
}
func toMutablse<S,T>(_ val0:[S], _ val1:T, _ fnc:(_:UnsafeMutablePointer<S>,_:UnsafeMutablePointer<T>)->Void)
{
	toMutabls(val0,{(ptr0:UnsafeMutablePointer<S>) in toMutable(val1,{(ptr1:UnsafeMutablePointer<T>) in fnc(ptr0,ptr1)})})
}
func toMutables<S,T>(_ val0:S, _ val1:[T], _ fnc:(_:UnsafeMutablePointer<S>,_:UnsafeMutablePointer<T>)->Void)
{
	toMutable(val0,{(ptr0:UnsafeMutablePointer<S>) in toMutabls(val1,{(ptr1:UnsafeMutablePointer<T>) in fnc(ptr0,ptr1)})})
}
func toMutablss<S,T>(_ val0:[S], _ val1:[T], _ fnc:(_:UnsafeMutablePointer<S>,_:UnsafeMutablePointer<T>)->Void)
{
	toMutabls(val0,{(ptr0:UnsafeMutablePointer<S>) in toMutabls(val1,{(ptr1:UnsafeMutablePointer<T>) in fnc(ptr0,ptr1)})})
}
func toPointre<T>(_ val:T, _ fnc:(_:UnsafePointer<T>)->Void)
{
	toMutable(val,{(val) in let ptr = UnsafePointer<T>(val); fnc(ptr)})
}
func toPointrs<T>(_ val:[T], _ fnc:(_:UnsafePointer<T>)->Void)
{
	toMutabls(val,{(val) in let ptr = UnsafePointer<T>(val); fnc(ptr)})
}
func toPointree<S,T>(_ val0:S, _ val1:T, _ fnc:(_:UnsafePointer<S>,_:UnsafePointer<T>)->Void)
{
	toMutablee(val0,val1,{(val0,val1) in let ptr0 = UnsafePointer<S>(val0); let ptr1 = UnsafePointer<T>(val1); fnc(ptr0,ptr1)})
}
func toPointrse<S,T>(_ val0:[S], _ val1:T, _ fnc:(_:UnsafePointer<S>,_:UnsafePointer<T>)->Void)
{
	toMutablse(val0,val1,{(val0,val1) in let ptr0 = UnsafePointer<S>(val0); let ptr1 = UnsafePointer<T>(val1); fnc(ptr0,ptr1)})
}
func toPointres<S,T>(_ val0:S, _ val1:[T], _ fnc:(_:UnsafePointer<S>,_:UnsafePointer<T>)->Void)
{
	toMutables(val0,val1,{(val0,val1) in let ptr0 = UnsafePointer<S>(val0); let ptr1 = UnsafePointer<T>(val1); fnc(ptr0,ptr1)})
}
func toPointrss<S,T>(_ val0:[S], _ val1:[T], _ fnc:(_:UnsafePointer<S>,_:UnsafePointer<T>)->Void)
{
	toMutablss(val0,val1,{(val0,val1) in let ptr0 = UnsafePointer<S>(val0); let ptr1 = UnsafePointer<T>(val1); fnc(ptr0,ptr1)})
}
