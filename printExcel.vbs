Dim XLApp
Dim XLWkbk
Dim ObjArgs

set ObjArgs = wscript.arguments
if ObjArgs.count <> 1 then
wscript.echo "Invalid passed arguments"
wscript.quit
end if

Set XLApp = CreateObject("Excel.Application")
XLApp.Visible = False

Set XLWkbk = XLApp.Workbooks.Open(objargs(0))
XLWkbk.PrintOut
XLWkbk.Close False

XLApp.Quit

Set XLWkbk = Nothing
Set XLApp = Nothing
Set ObjArgs = nothing