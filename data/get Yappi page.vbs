dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

Set fso = CreateObject("Scripting.FileSystemObject")
Set fl = fso.OpenTextFile("C:\Users\A097092\Desktop\Extra\HS Football\Yappi\Yappi_Links.txt",1)


do until fl.atendofstream
i = fl.readline

xHttp.Open "GET", "http://yappi.net" & i, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\HS Football\Yappi\Historical Results\" & right(i,len(i)-22) & ".txt", 2
    .close
end with

loop

fl.close

Set fl = Nothing
Set fso = Nothing
Set bStrm = Nothing
Set xHttp = Nothing

msgbox "Done"






