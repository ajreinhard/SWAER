dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

Set fso = CreateObject("Scripting.FileSystemObject")
Set fl = fso.OpenTextFile("C:\Users\Owner\Desktop\SWAER\helmets\non_oh_helm_pics.txt",1)

do until fl.atendofstream
i = fl.readline

xHttp.Open "GET", i, False
xHttp.Send

i = fl.readline

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\Owner\Desktop\SWAER\helmets\pics\" & i & ".png", 2
    .close
end with

loop

fl.close

Set fl = Nothing
Set fso = Nothing
Set bStrm = Nothing
Set xHttp = Nothing

msgbox "Done"






