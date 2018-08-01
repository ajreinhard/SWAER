        strRequest = "{""method"":""games.search"",""params"":{""isExploreSearch"":true,""aboveConfidenceGrade"":30,""afterDateTime"":""2018-06-04 14:59:13"",""beforeDateTime"":""2018-06-12 14:59:13"",""location"":{""country"":""US"",""city"":""Cleveland"",""latitude"":41.4995,""longitude"":-81.6954,""state"":""OH""},""sportNames"":[""football""],""squadIds"":[1010,1020,1030],""country"":""US"",""state"":""OH"",""offset"":0,""count"":50,""apiKey"":""a20bd983-0147-437a-ab6d-49afeb883d33""}}"
        EndPointLink = "https://scorestream.com/api"



dim http
set http=createObject("Microsoft.XMLHTTP")
http.open "POST",EndPointLink,false
http.setRequestHeader "Content-Type","application/json"

msgbox "REQUEST : " & strRequest
http.send strRequest

If http.Status = 200 Then
'msgbox "RESPONSE : " & http.responseXML.xml
msgbox "RESPONSE : " & http.responseText
responseText=http.responseText
else
msgbox "ERRCODE : " & http.status
End If