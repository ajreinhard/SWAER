dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

Set fso = CreateObject("Scripting.FileSystemObject")
Set fl = fso.OpenTextFile("C:\Users\A097092\Desktop\Extra\HS Football\MaxPreps\2018 sched.txt",1)

j = 1
do until fl.atendofstream
i = fl.readline
if left(i,4) <> "http" then i = "http://www.maxpreps.com/high-schools/" & i & "/football/schedule.htm"

xHttp.Open "GET", i, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\HS Football\MaxPreps\Sched18\" & j & ".txt", 2
    .close
end with

j = j+1
loop

fl.close

Set fl = Nothing
Set fso = Nothing
Set bStrm = Nothing
Set xHttp = Nothing

msgbox "Done"




