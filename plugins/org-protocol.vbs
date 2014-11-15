'**************************************
' Name: URLEncode Function
' Description:Encodes a string to create legally formatted
'QueryString for URL. This function is more flexible
'than the IIS Server.Encode function because you can
'pass in the WHOLE URL and only the QueryString data
'will be converted. IIS strangely converts EVERYTHING
'(ie "http://" becomes "http%3A%2F%2F").
' By: Markus Diersbock
'
' Inputs:sRawURL - String to Encode
'
' Returns:Encoded String
'
'This code is copyrighted and has' limited warranties.
'Please see http://www.Planet-Source-Code.com/vb/scripts/ShowCode.asp?txtCodeId=43806&lngWId=1'for details.
'**************************************

' Changed by Matthew Fidler to have http:// become http%3A%2F%2F
' Also changed to have spaces be %20 instead of +


Public Function URLEncode(sRawURL As String) As String
    On Error GoTo Catch
    Dim iLoop As Integer
    Dim sRtn As String
    Dim sTmp As String
    Const sValidChars = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    If Len(sRawURL) > 0 Then
        ' Loop through each char
        For iLoop = 1 To Len(sRawURL)
            sTmp = Mid(sRawURL, iLoop, 1)
            If InStr(1, sValidChars, sTmp, vbBinaryCompare) = 0 Then
                ' If not ValidChar, convert to HEX and prefix with %
                sTmp = Hex(Asc(sTmp))
                If Len(sTmp) = 1 Then
                    sTmp = "%0" & sTmp
                Else
                    sTmp = "%" & sTmp
                End If
            End If
            sRtn = sRtn & sTmp
        Next iLoop
        URLEncode = sRtn
    End If
Finally:
    Exit Function
Catch:
    URLEncode = ""
    Resume Finally
End Function

' From http://www.freevbcode.com/ShowCode.asp?ID=3476
Public Function OutlookFolderNames(objFolder As Outlook.MAPIFolder, strFolderName As String) As Object
'*********************************************************
    On Error GoTo ErrorHandler
    Dim objOneSubFolder As Outlook.MAPIFolder
    If Not objFolder Is Nothing Then
        If LCase(strFolderName) = LCase(objFolder.Name) Then
            Set OutlookFolderNames = objFolder
        Else
            ' Check if folders collection is not empty
            If objFolder.Folders.Count > 0 And _
                   Not objFolder.Folders Is Nothing Then
                For Each oFolder In objFolder.Folders
                    Set objOneSubFolder = oFolder
                    ' only check mail item folder
                    If objOneSubFolder.DefaultItemType _
                         = olMailItem Then
                        If LCase(strFolderName) = _
                          LCase(objOneSubFolder.Name) Then
                            Set OutlookFolderNames = _
                                   objOneSubFolder
                            Exit For
                        Else
                            If objOneSubFolder.Folders.Count _
                                > 0 Then
                                Set OutlookFolderNames = _
                                  OutlookFolderNames _
                                (objOneSubFolder, strFolderName)
                            End If
                        End If
                    End If
                Next
            End If
        End If
    End If

    Exit Function

ErrorHandler:
    Set OutlookFolderNames = Nothing
End Function


Sub CreateTaskFromItem()
    Dim T As Variant
    Dim SndName As String
    Dim SndEmailAddress As String
    Dim Outlook As New Outlook.Application
    Dim allPersonalFolders As Outlook.MAPIFolder
    
    
    ' Send selected text to clipboard.
    ' SendKeys ("%E")
    ' SendKeys ("C")
    ' DoEvents
    
    
    Set objWeb = CreateObject("InternetExplorer.Application")
    
        
    If Outlook.Application.ActiveExplorer.Selection.Count > 0 Then
        For i = 1 To Outlook.Application.ActiveExplorer.Selection.Count
            Set objMail = Outlook.ActiveExplorer.Selection.Item(i)
            On Error GoTo BlockedSnd
            SndName = ObjMail.SenderName
            SndEmailAddress = ObjMail.SenderEmailAddress
            GoTo SndDone
BlockedSnd:
            SndName = "Blocked"
            SndEmailAddress = "Blocked@Microsoft.com"
SndDone:
            
            On Error GoTo 0
            objMail.Save 'Maybe this will update EntryID
            T = "org-protocol:/outlook:/o/" + URLEncode(objMail.EntryID) _
                    + "/" + URLEncode(objMail.Subject) _
                    + "/" + URLEncode(SndName) _
                    + "/" + URLEncode(SndEmailAddress) _
                    '+ "/" + URLEncode(objMail.Body)
            objWeb.Navigate T
            objWeb.Visible = True
        Next
    End If
End Sub