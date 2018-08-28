dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

Set fso = CreateObject("Scripting.FileSystemObject")
Set fl = fso.OpenTextFile("C:\Users\Owner\Desktop\SWAER\output\2018\JE Teams.txt",1)


do until fl.atendofstream
i = fl.readline

xHttp.Open "GET", "http://www.joeeitel.com/hsfoot/teams.jsp?year=2018&teamID=" & i, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\Owner\Desktop\SWAER\output\2018\JE Games\" & i & ".txt", 2
    .close
end with

loop

fl.close

Set fl = Nothing
Set fso = Nothing
Set bStrm = Nothing
Set xHttp = Nothing

msgbox "Done"






