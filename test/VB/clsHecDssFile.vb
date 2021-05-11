Option Strict Off
Option Explicit On
Imports System.Globalization
Imports System.Runtime.InteropServices

Public Class HecDssFile
    Implements IDisposable
    Private cstrMyName As String = ""
    Private mlngIfltab(1000) As Integer
    Private Const Cunit As Integer = 13
    Private mstrMessageFilename As String
    Public SuppressErrors As Boolean

    Private MyProcessID As Integer = Process.GetCurrentProcess().Id

    Public Function Ifltab() As Integer()
        Return mlngIfltab
    End Function

    Public Function CopyDSSRecord(ByVal FromPathname As String, ToPathname As String, Optional ByVal DestDB As HecDssFile = Nothing) As Integer
        Dim istat As Integer
        If DestDB Is Nothing Then
            istat = hecZcorec(mlngIfltab, mlngIfltab, FromPathname, ToPathname)
        Else
            istat = hecZcorec(mlngIfltab, DestDB.Ifltab, FromPathname, ToPathname)
        End If
        Return istat
    End Function

    Private Function hecZcorec(ByRef iftold() As Integer, ByRef addressOfIftnew() As Integer, ByVal cpold As String, ByVal cpnew As String) As Integer
        Const kbuff1 As Integer = 8192
        Const kbuff2 As Integer = 100
        Dim intRet As Integer
        Dim buff1(kbuff1) As Single
        Dim buff2(kbuff2) As Single
        SafeNative.heclib.ZCOREC_(iftold(0), addressOfIftnew(0), cpold, Len(cpold), cpnew, Len(cpnew), buff1, kbuff1, buff2, kbuff2, intRet)
        Return intRet
    End Function


    Public Function VarPtr(ByVal e As Object) As Integer
        Dim GC As GCHandle = GCHandle.Alloc(e, GCHandleType.Pinned)
        Dim GC2 As Integer = GC.AddrOfPinnedObject.ToInt32
        GC.Free()
        Return GC2
    End Function

    Public Sub ZSET(ByVal strItem As String, ByVal varSetting As String)
        Dim lngSetting As Integer
        Dim strSetting As String = ""
        Dim dbNumericTemp As Double
        If Double.TryParse(varSetting, NumberStyles.Number, CultureInfo.CurrentCulture.NumberFormat, dbNumericTemp) Then
            lngSetting = CInt(varSetting)
            SafeNative.heclib.ZSET_(strItem, CStr(strItem.Length), "", CStr(0), lngSetting)
        Else
            strSetting = "" & varSetting
            SafeNative.heclib.ZSET_(strItem, CStr(strItem.Length), strSetting, CStr(strSetting.Length), 0)
        End If
    End Sub
    Private Function FORTRANOPEN(ByRef strFileName As String, ByRef lngUnit As Integer) As Integer
        Return SafeNative.heclib.FORTRANOPEN_(strFileName, strFileName.Length, lngUnit)
    End Function
    Private Function FORTRANCLOSE(ByRef lngUnit As Integer) As Integer
        Return SafeNative.heclib.FORTRANCLOSE_(lngUnit)
    End Function
    Public Function ZOPEN(ByVal DSSFileName As String) As Integer
        Dim ISTAT As Integer
        AttachMessageFile()
        DSSFileName = DSSFileName.Trim()
        SafeNative.heclib.ZOPEN_(mlngIfltab(mlngIfltab.GetLowerBound(0)), DSSFileName, DSSFileName.Length, ISTAT)
        Return ISTAT
    End Function
    Public Sub ZCLOSE()
        SafeNative.heclib.ZCLOSE_(mlngIfltab(mlngIfltab.GetLowerBound(0)))
    End Sub
    Public Function ZRRTS(ByRef path As String, ByRef StartDateTime As Date, ByRef theData() As Single,
                          ByRef theUnits As String, ByRef theType As String, ByRef intOffset As Integer, ByRef ISTAT As Integer) As Integer
        Dim asciiEncoder As System.Text.Encoding = New System.Text.ASCIIEncoding
        Dim iOfset As Integer
        Dim nvals As Integer = theData.GetUpperBound(0) - theData.GetLowerBound(0) + 1
        ISTAT = 0
        Dim theCUnits() As Byte = New Byte((8) - 1) {}
        Dim theCType() As Byte = New Byte((8) - 1) {}
        Dim getCulture As CultureInfo = CultureInfo.CreateSpecificCulture("en-US")
        Dim startDate As String = Date.Parse(StartDateTime).ToString("ddMMMyyyy", getCulture)
        Dim startTime As String = Date.Parse(StartDateTime).ToString("HHmm", getCulture)

        SafeNative.heclib.ZRRTS_(mlngIfltab(mlngIfltab.GetLowerBound(0)), path, path.Length, startDate,
                                                 startDate.Length, startTime, startTime.Length, nvals, theData(0), theCUnits, theCUnits.Length, theCType,
                                                 theCType.Length, iOfset, ISTAT)

        Dim converter As String = asciiEncoder.GetString(theCUnits)
        intOffset = iOfset
        theUnits = converter.Trim
        converter = asciiEncoder.GetString(theCType)
        theType = converter.Trim


        Return nvals
    End Function



    Public Sub MakeCat(ByVal CatFilename As String)
        Dim lngNoRecs As Integer

        FORTRANCLOSE(Cunit)
        Dim lngErrorCode As Integer = FORTRANOPEN(CatFilename, 13)
        If lngErrorCode <> 0 Then Information.Err().Raise(&H4000S + lngErrorCode)
        ' sort catalog
        SafeNative.heclib.ZCAT_(mlngIfltab(0), Cunit, 0, 0, " ", 1, 1, 1, 0, lngNoRecs)
        If lngErrorCode <> 0 Then Information.Err().Raise(&H4000S + lngErrorCode)
        FORTRANCLOSE(Cunit)
    End Sub

    Public Sub ZSRTS(ByRef strCPATH As String, ByRef StartDateTime As Date, ByRef sngValues() As Single, ByRef strCunits As String,
                     ByRef strCtype As String, ByRef lngIplan As Integer, ByRef lngIstat As Integer)

        Dim getCulture As CultureInfo = CultureInfo.CreateSpecificCulture("en-US")
        Dim strStartDate As String = StartDateTime.ToString("ddMMMyyyy", getCulture)
        Dim strStartTime As String = StartDateTime.ToString("HHmm", getCulture)
        Dim lngNVals As Integer = sngValues.GetUpperBound(0) - sngValues.GetLowerBound(0) + 1
        SafeNative.heclib.ZSRTS_(mlngIfltab(mlngIfltab.GetLowerBound(0)), strCPATH, strCPATH.Length, strStartDate, strStartDate.Length, strStartTime,
                                                 strStartTime.Length, lngNVals, sngValues(sngValues.GetLowerBound(0)), strCunits, strCunits.Length, strCtype, strCtype.Length, lngIplan, lngIstat)
    End Sub
    Public Sub ZSPD(ByRef strCPATH As String, ByRef lngNord As Integer, ByRef lngNcurve As Integer, ByRef lngIhoriz As Integer,
                         ByRef strC1unit As String, ByRef strC1type As String, ByRef strC2unit As String, ByRef strC2type As String,
                         ByRef sngValues() As Single, ByRef strClabel() As String, ByRef lngIplan As Integer,
                         ByRef lngIstat As Integer)

        Dim strLabels() As String
        Dim strCLabels As String = ""
        Dim lngLenCLabels As Integer
        Dim lngLabelsPresent As Integer

        If strClabel.GetUpperBound(0) >= 0 Then
            ReDim strLabels(lngNcurve - 1)
            For i As Integer = 0 To lngNcurve - 1
                strLabels(i) = strClabel(i).Substring(0, Math.Min(12, strClabel(i).Length)).PadRight(12)
                strCLabels = strCLabels & strLabels(i)
            Next
            lngLenCLabels = 12
            lngLabelsPresent = 1
        Else
            lngLenCLabels = 0
            lngLabelsPresent = 0
        End If

        Dim sngHeadu As Integer = 0
        Dim lngNHeadu As Integer = 0
        Dim temp(sngValues.GetLength(0) - 1) As Single
        Array.Copy(sngValues, temp, sngValues.Length)

        SafeNative.heclib.ZSPD_(mlngIfltab(mlngIfltab.GetLowerBound(0)), strCPATH, strCPATH.Length, lngNord, lngNcurve,
                                                  lngIhoriz, strC1unit, strC1unit.Length, strC1type, strC1type.Length, strC2unit,
                                                  strC2unit.Length, strC2type, strC2type.Length, temp(temp.GetLowerBound(0)), strCLabels,
                                                  lngLenCLabels, lngLabelsPresent, sngHeadu, lngNHeadu, lngIplan, lngIstat)
        Array.Copy(temp, sngValues, temp.Length)
        ' TBD: Error handling
        '    If Not SuppressErrors Then
        '        If istat > 5 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: severe error, code ", istat
        '        If istat <> 0 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: some or all data missing"
        '        If nvals <> UBound(theData) - LBound(theData) + 1 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: retrieved unexpected number of values"
        '    End If
    End Sub

    Public Sub ZRPDD(ByRef strCPATH As String, ByRef lngNord As Integer, ByRef lngNcurve As Integer,
                         ByRef lngIhoriz As Integer, ByRef strC1unit As String, ByRef strC1type As String,
                         ByRef strC2unit As String, ByRef strC2type As String, ByRef dblValues() As Double,
                         ByRef strClabel() As String, ByRef lngIplan As Integer, ByRef lngIstat As Integer)
        Dim asciiEncoder As System.Text.Encoding = New System.Text.ASCIIEncoding
        Const KLABEL As Integer = 50

        Dim Label As Boolean
        Dim CLABEL() As String

        Dim lngLabel As Integer
        Dim lngL_Clabel As Short = 12
        'Dim sngHeadu As Single
        Dim lngHeadu, lngNHeadu, lngHKeadU, lngNVals As Integer

        Dim strCLabels As String = Space(KLABEL * lngL_Clabel)

        '   Read in the Paired Data
        Dim xUnitsBA() As Byte = New Byte((8) - 1) {}
        Dim yUnitsBA() As Byte = New Byte((8) - 1) {}
        Dim xTypeBA() As Byte = New Byte((8) - 1) {}
        Dim yTypeBA() As Byte = New Byte((8) - 1) {}
        Dim labelBA() As Byte = New Byte((30) - 1) {}
        Dim sngValues() As Single
        ReDim sngValues(dblValues.Length - 1)
        SafeNative.heclib.ZRPD_(mlngIfltab(mlngIfltab.GetLowerBound(0)), strCPATH, strCPATH.Length, lngNord, lngNcurve, lngIhoriz, xUnitsBA,
                                                xUnitsBA.GetLength(0), xTypeBA, xTypeBA.GetLength(0), yUnitsBA, yUnitsBA.GetLength(0), yTypeBA, yTypeBA.GetLength(0),
                                                sngValues(sngValues.GetLowerBound(0)), sngValues.GetUpperBound(0), lngNVals,
                                                labelBA, lngL_Clabel, KLABEL, lngLabel, lngHeadu, lngHKeadU, lngNHeadu, lngIstat)
        For i As Integer = 0 To sngValues.Length - 1
            dblValues(i) = sngValues(i)
        Next


        'dblValues = Array.ConvertAll(Of Single, Double)(sngValues, AddressOf DoubleConverter)

        ' Copy the units and data type strings
        strC1unit = asciiEncoder.GetString(xUnitsBA).Trim
        strC2unit = asciiEncoder.GetString(yUnitsBA).Trim
        strC1type = asciiEncoder.GetString(xTypeBA).Trim
        strC2type = asciiEncoder.GetString(yTypeBA).Trim

        ' copy the labels
        If lngLabel > 0 Then
            ' labels were returned
            Label = True
            ReDim CLABEL(lngNcurve)
            For i As Integer = 1 To lngNcurve
                CLABEL(i) = RTrim(Mid(asciiEncoder.GetString(labelBA), 12 * (i - 1) + 1, 12))
            Next i
        Else
            Label = False
        End If
    End Sub

    Public Sub ZRPD(ByRef strCPATH As String, ByRef lngNord As Integer, ByRef lngNcurve As Integer,
                         ByRef lngIhoriz As Integer, ByRef strC1unit As String, ByRef strC1type As String,
                         ByRef strC2unit As String, ByRef strC2type As String, ByRef sngValues() As Single,
                         ByRef strClabel() As String, ByRef lngIplan As Integer, ByRef lngIstat As Integer)
        Dim asciiEncoder As System.Text.Encoding = New System.Text.ASCIIEncoding
        Const KLABEL As Integer = 50

        Dim Label As Boolean
        Dim CLABEL() As String

        Dim lngLabel As Integer
        Dim lngL_Clabel As Short = 12
        'Dim sngHeadu As Single
        Dim lngHeadu, lngNHeadu, lngHKeadU, lngNVals As Integer

        Dim strCLabels As String = Space(KLABEL * lngL_Clabel)

        '   Read in the Paired Data
        Dim xUnitsBA() As Byte = New Byte((8) - 1) {}
        Dim yUnitsBA() As Byte = New Byte((8) - 1) {}
        Dim xTypeBA() As Byte = New Byte((8) - 1) {}
        Dim yTypeBA() As Byte = New Byte((8) - 1) {}
        Dim labelBA() As Byte = New Byte((30) - 1) {}


        SafeNative.heclib.ZRPD_(mlngIfltab(mlngIfltab.GetLowerBound(0)), strCPATH, strCPATH.Length, lngNord, lngNcurve, lngIhoriz, xUnitsBA,
                                                xUnitsBA.GetLength(0), xTypeBA, xTypeBA.GetLength(0), yUnitsBA, yUnitsBA.GetLength(0), yTypeBA, yTypeBA.GetLength(0),
                                                sngValues(sngValues.GetLowerBound(0)), sngValues.GetUpperBound(0), lngNVals,
                                                labelBA, lngL_Clabel, KLABEL, lngLabel, lngHeadu, lngHKeadU, lngNHeadu, lngIstat)

        ' Copy the units and data type strings
        strC1unit = asciiEncoder.GetString(xUnitsBA).Trim
        strC2unit = asciiEncoder.GetString(yUnitsBA).Trim
        strC1type = asciiEncoder.GetString(xTypeBA).Trim
        strC2type = asciiEncoder.GetString(yTypeBA).Trim

        ' copy the labels
        If lngLabel > 0 Then
            ' labels were returned
            Label = True
            ReDim CLABEL(lngNcurve)
            For i As Integer = 1 To lngNcurve
                CLABEL(i) = RTrim(Mid(asciiEncoder.GetString(labelBA), 12 * (i - 1) + 1, 12))
            Next i
        Else
            Label = False
        End If
    End Sub

    Public Function ZDTYPE(ByRef strCPATH As String) As Boolean

        Dim lngNsize, lngLexist, lngIDtype As Integer
        Dim DTypeBA() As Byte = New Byte((3) - 1) {}

        SafeNative.heclib.ZDTYPE_(mlngIfltab(mlngIfltab.GetLowerBound(0)), strCPATH, strCPATH.Length,
                                                  lngNsize, lngLexist, DTypeBA, DTypeBA.GetLength(0), lngIDtype)

        ' TBD: Error handling
        '    If Not SuppressErrors Then
        '        If istat > 5 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: severe error, code ", istat
        '        If istat <> 0 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: some or all data missing"
        '        If nvals <> UBound(theData) - LBound(theData) + 1 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: retrieved unexpected number of values"
        '    End If

        Return Not (lngLexist = 0)
    End Function

    Public Function ZDELET(ByRef strCPATH As String) As Boolean

        Dim lngLfound As Integer

        SafeNative.heclib.ZDELET_(mlngIfltab(mlngIfltab.GetLowerBound(0)), strCPATH, strCPATH.Length, strCPATH.Length, lngLfound)

        ' TBD: Error handling
        '    If Not SuppressErrors Then
        '        If istat > 5 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: severe error, code ", istat
        '        If istat <> 0 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: some or all data missing"
        '        If nvals <> UBound(theData) - LBound(theData) + 1 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: retrieved unexpected number of values"
        '    End If

        Return Not (lngLfound = 0)
    End Function

    Public Function ZINQIR(ByRef strCitem As String, ByRef strCstr As String, ByRef lngInumb As Integer) As Boolean
        Dim asciiEncoder As System.Text.Encoding = New System.Text.ASCIIEncoding
        Dim cString() As Byte = New Byte((8) - 1) {}

        lngInumb = -903
        SafeNative.heclib.ZINQIR_(mlngIfltab(mlngIfltab.GetLowerBound(0)), strCitem,
                                                  strCitem.Length, cString, cString.GetLength(0), lngInumb)


        'If lngInumb = -903 Then
        strCstr = asciiEncoder.GetString(cString).Trim
        ' Else
        ' strCstr = CStr(lngInumb)
        ' End If

        ' TBD: Error handling
        '    If Not SuppressErrors Then
        '        If istat > 5 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: severe error, code ", istat
        '        If istat <> 0 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: some or all data missing"
        '        If nvals <> UBound(theData) - LBound(theData) + 1 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: retrieved unexpected number of values"
        '    End If

        Return True
    End Function

    'TBD: Fix this!
    Public Function SQUEEZEDSS(ByRef strCPATH As String) As Integer
        Dim lngIstat As Integer
        '    SQUEEZEDSS_ 6
        SafeNative.heclib.SQUEEZEDSS_(strCPATH, strCPATH.Length, lngIstat)

        ' TBD: Error handling
        '    If Not SuppressErrors Then
        '        If istat > 5 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: severe error, code ", istat
        '        If istat <> 0 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: some or all data missing"
        '        If nvals <> UBound(theData) - LBound(theData) + 1 Then _
        ''            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: retrieved unexpected number of values"
        '    End If

        '    SQUEEZEDSS = 0
        Return lngIstat
    End Function

    Public Sub New()
        MyBase.New()
        'ZSET("DSSV", 6)
        'cstrMyName = EFM_RM.GetString("01265")
        'Dim ISTAT As Integer = FORTRANOPEN(Interaction.Environ("TMP") & "\" & "heclib_console.txt", 6)
        'If ISTAT <> 0 Then Information.Err().Raise(10000 + ISTAT + Constants.vbObjectError, , "Unable to open console log file")
    End Sub

    Public Sub Dispose() Implements IDisposable.Dispose
        ZCLOSE()
        FORTRANCLOSE(6)
        If IO.File.Exists(mstrMessageFilename) Then
            Try
                IO.File.Delete(mstrMessageFilename)
            Catch ex As Exception
                'Could not delete ignore
            End Try
        End If
        'If Not IsNothing(EFM_RM) Then
        '    EFM_RM.ReleaseAllResources()
        '    EFM_RM = Nothing
        'End If
        FORTRANCLOSE(Cunit)
        'Added for problem in PathPicker
        GC.SuppressFinalize(Me)
    End Sub

    Public Function IsConnected() As Boolean
        IsConnected = mlngIfltab(0) = 6
    End Function


    Private Sub AttachMessageFile()
        Dim strTempFolder As String
        Dim lngStatus As Long

        Dim intTrial As Integer = MyProcessID



        Dim MAX_INST As Integer = MyProcessID + 20

        'Routine to establish DSS message levels and output file
        'Should only be called once at the beginning of operations for this instance of the server
        strTempFolder = Space(255)
        'GetTempPath(Len(strTempFolder), strTempFolder)
        strTempFolder = System.IO.Path.GetTempPath
        'strTempFolder = Left(strTempFolder, InStr(1, strTempFolder, Chr(0)) - 1)
        If Len(mstrMessageFilename) = 0 Then
            Do
                mstrMessageFilename = strTempFolder & "~EFMDSS_messages_" & CStr(intTrial) & ".txt"
                Try
                    If Not IsFileLocked(mstrMessageFilename) Then
                        lngStatus = FORTRANOPEN(mstrMessageFilename, 6)
                    Else
                        lngStatus = 1
                    End If
                Catch ex As Exception
                    lngStatus = 1
                End Try

                'opendssoutput_ mstrMessageFilename, Len(mstrMessageFilename), lngStatus
                'lngStatus = FORTRANOPEN(mstrMessageFilename, 6)
                intTrial = intTrial + 1
            Loop Until lngStatus = 0 Or intTrial >= MAX_INST
            'If intTrial exceeds MAX_INST, the file won't be opened, but instead it will attempt to write to Fort.69
        End If
    End Sub

    Private Function IsFileLocked(FileName As String) As Boolean
        Dim retval As Boolean = False
        Try
            If IO.File.Exists(FileName) Then
                'CREATE A FILE STREAM FROM THE FILE, OPENING IT FOR READ ONLY EXCLUSIVE ACCESS
                Dim FS As IO.FileStream = IO.File.Open(FileName, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.None)
                'CLOSE AND CLEAN UP RIGHT AWAY, IF THE OPEN SUCCEEDED, WE HAVE OUR ANSWER ALREADY
                FS.Close()
                FS.Dispose()
                FS = Nothing
            End If
        Catch ex As Exception
            retval = True
        End Try


        Return retval


    End Function
End Class
