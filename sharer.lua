-- if #arg > 0 then write and read some type, write again to terminate shareC, and exit successfully
-- if #arg == 0 then start share with Execute of this script and Fanout to itself and ""
if not os.execute("gtimeout 1s ./shareC 'Fanout(siz:Int(1)dst[0]:Str()typ:Str(Persist)str:Str(fanout))'") then io.stderr:write("sharerLua: cannot execute file: shareC\n"); os.exit(-1) end
