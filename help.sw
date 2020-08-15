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
func fromZero<T>() -> T
{
	return fromZero(1)[0]
}
func fromZero<T>(_ len:Int) -> [T]
{
	let siz = MemoryLayout<Pierce>.size
	let ptr = UnsafeMutablePointer<Int8>.allocate(capacity:len*siz)
	var count = 0
	while (count < len*siz) {
		ptr[count] = 0
		count = count + 1
	}
	let res:[T] = fromRaw(ptr,len)
	ptr.deallocate()
	return res
}
func toList<T>(_ val:T, _ len:Int) -> [T]
{
	var vals:[T] = []
	while (vals.count < len) {
		vals.append(val)
	}
	return vals
}
func fromRaw<T>(_ raw:UnsafeRawPointer) -> T
{
	return fromRaw(raw,1)[0]
}
func fromRaw<T>(_ raw:UnsafeRawPointer, _ len:Int) -> [T]
{
	let siz = MemoryLayout<T>.size
	var current = raw
	var vals:[T] = []
	while (vals.count < len) {
		current = current.advanced(by:siz)
		vals.append(current.load(as:T.self))
	}
	return vals
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
	var count = 0
	for val in list {
		ptr[count] = val
		count = count + 1
	}
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
