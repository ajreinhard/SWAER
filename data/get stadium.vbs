dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

Set fso = CreateObject("Scripting.FileSystemObject")
Set fl = fso.OpenTextFile("C:\Users\A097092\Desktop\Extra\HS Football\Stadium\stadium ID.txt",1)

do until fl.atendofstream
i = fl.readline

xHttp.Open "GET", "http://ohiostadiums.com/stadium.php?id=" & i, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\HS Football\Stadium\pages\" & i & ".txt", 2
    .close
end with

loop

fl.close

Set fl = Nothing
Set fso = Nothing
Set bStrm = Nothing
Set xHttp = Nothing

msgbox "Done"






