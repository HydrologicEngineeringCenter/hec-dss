Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Specialized
Imports System.Windows.Forms
Namespace SafeNative
    Public Module heclib
        Public Function FORTRANCLOSE_(ByRef lngUnit As Integer) As Integer
            Return UnsafeNative.heclib.FORTRANCLOSE_(lngUnit)
        End Function

        Public Function FORTRANOPEN_(ByRef strFileName As String, ByVal lngLenFN As Integer, ByRef lngUnit As Integer) As Integer
            Return UnsafeNative.heclib.FORTRANOPEN_(strFileName, lngUnit, lngLenFN)
        End Function

        Public Sub SQUEEZEDSS_(ByRef strCPATH As String, ByVal lngL_CPath As Integer, ByRef ISTAT As Integer)
            UnsafeNative.heclib.SQUEEZEDSS_(strCPATH, ISTAT, lngL_CPath)
        End Sub

        Public Sub ZCAT_(ByRef lngIfltab As Integer, ByRef lngICUNIT As Integer, ByRef lngICDUNT As Integer, ByRef lngINUNIT As Integer, ByRef strCINSTR As String, ByVal lngL_CINSTR As Integer, ByRef lngLABREV As Integer, ByRef lngLDOSRT As Integer, ByRef lngLCDCAT As Integer, ByRef lngNoRecs As Integer)
            UnsafeNative.heclib.ZCAT_(lngIfltab, lngICUNIT, lngICDUNT, lngINUNIT, strCINSTR, lngL_CINSTR, lngLABREV, lngLDOSRT, lngLCDCAT, lngNoRecs)
        End Sub

        Public Sub ZCLOSE_(ByRef ifltab As Integer)
            UnsafeNative.heclib.ZCLOSE_(ifltab)
        End Sub

        Public Sub ZDELET_(ByRef ifltab As Integer, ByRef strCPATH As String, ByVal lngL_CPath As Integer, ByRef lngNPath As Integer, ByRef lngLfound As Integer)
            UnsafeNative.heclib.ZDELET_(ifltab, strCPATH, lngNPath, lngLfound, lngL_CPath)
        End Sub

        Public Sub ZDTYPE_(ByRef ifltab As Integer, ByRef strCPATH As String, ByVal lngL_CPath As Integer, ByRef lngNsize As Integer, ByRef lngLexist As Boolean, _
                           ByRef strCDtype() As Byte, ByVal lngL_CDtype As Integer, ByRef lngIDtype As Integer)
            UnsafeNative.heclib.ZDTYPE_(ifltab, strCPATH, lngNsize, lngLexist, strCDtype, lngIDtype, lngL_CPath, lngL_CDtype)
        End Sub

        Public Sub ZINQIR_(ByRef ifltab As Integer, ByRef strCitem As String, ByVal lngL_CItem As Integer, ByRef strCstr() As Byte, _
                           ByVal lngL_Cstr As Integer, ByRef lngInumb As Integer)
            UnsafeNative.heclib.ZINQIR_(ifltab, strCitem, strCstr, lngInumb, lngL_CItem, lngL_Cstr)
        End Sub

        Public Sub ZOPEN_(ByRef ifltab As Integer, ByRef strFileName As String, ByVal lngLenFN As Integer, ByRef lngIstat As Integer)
            UnsafeNative.heclib.ZOPEN_(ifltab, strFileName, lngIstat, lngLenFN)
        End Sub

        Public Sub ZRPD_(ByRef lngIfltab As Integer, ByRef strPath As String, ByVal lngL_Path As Integer, ByRef lngNord As Integer, _
                         ByRef lngNcurve As Integer, ByRef lngHoriz As Integer, ByRef str1Unit() As Byte, ByVal lngL_1Unit As Integer, _
                         ByRef str1Type() As Byte, ByVal lngL_1Type As Integer, ByRef str2Unit() As Byte, ByVal lngL_2Unit As Integer, _
                         ByRef str2Type() As Byte, ByVal lngL_2Type As Integer, ByRef sngValues As Single, ByRef lngKVals As Integer, _
                         ByRef lngNVals As Integer, ByRef strLabel() As Byte, ByVal lngL_Label As Integer, ByRef lngKLabel As Integer, _
                         ByRef lngLabel As Integer, ByRef lngHeadu As Integer, ByRef lngHKeadU As Integer, ByRef lngNHeadu As Integer, ByRef lngStat As Integer)
            UnsafeNative.heclib.ZRPD_(lngIfltab, strPath, lngNord, lngNcurve, lngHoriz, str1Unit, str1Type, str2Unit, str2Type, _
                                        sngValues, lngKVals, lngNVals, strLabel, lngKLabel, lngLabel, lngHeadu, lngHKeadU, lngNHeadu, _
                                        lngStat, lngL_Path, lngL_1Unit, lngL_1Type, lngL_2Unit, lngL_2Type, lngL_Label)
        End Sub

        Public Sub ZRRTS_(ByRef ifltab As Integer, ByRef strCPATH As String, ByVal lngL_CPath As Integer, ByRef strCDate As String,
                          ByVal lngL_CDate As Integer, ByRef strCTime As String, ByVal lngL_CTime As Integer, ByRef lngNVals As Integer,
                          ByRef sngValues As Single, ByRef strCunits() As Byte, ByVal lngL_CUnits As Integer, ByRef strCtype() As Byte,
                          ByVal lngL_CType As Integer, ByRef lngIOFSET As Integer, ByRef lngIstat As Integer)
            UnsafeNative.heclib.ZRRTS_(ifltab, strCPATH, strCDate, strCTime, lngNVals, sngValues, _
                                          strCunits, strCtype, lngIOFSET, lngIstat, lngL_CPath, lngL_CDate, lngL_CTime, lngL_CUnits, lngL_CType)
        End Sub

        Public Sub ZSET_(ByRef citem As String, ByRef citem_len As Integer, ByRef ccstr As String, ByRef ccstr_len As Integer, ByRef inumb As Integer)
            UnsafeNative.heclib.ZSET_(citem, ccstr, inumb, citem_len, ccstr_len)
        End Sub

        Public Sub ZSPD_(ByRef ifltab As Integer, ByRef strCPATH As String, ByVal lngL_CPath As Integer, ByRef lngNord As Integer, ByRef lngNcurve As Integer, ByRef lngIhoriz As Integer, ByRef strC1Units As String, ByVal lngL_C1Units As Integer, ByRef strC1type As String, ByVal lngL_C1Type As Integer, ByRef strC2Units As String, ByVal lngL_C2Units As Integer, ByRef strC2type As String, ByVal lngL_C2Type As Integer, ByRef sngValues As Single, ByRef strClabel As String, ByVal lngL_Clabel As Integer, ByRef lngLabel As Integer, ByRef sngHeadu As Integer, ByRef lngNHeadu As Integer, ByRef lngIplan As Integer, ByRef lngIstat As Integer)
            UnsafeNative.heclib.ZSPD_(ifltab, strCPATH, lngNord, lngNcurve, lngIhoriz, strC1Units, strC1type, strC2Units, strC2type, sngValues, strClabel, lngLabel, sngHeadu, _
                                        lngNHeadu, lngIplan, lngIstat, lngL_CPath, lngL_C1Units, lngL_C1Type, lngL_C2Units, lngL_C2Type, lngL_Clabel)
        End Sub

        Public Sub ZSRTS_(ByRef ifltab As Integer, ByRef strCPATH As String, ByVal lngL_CPath As Integer, ByRef strCDate As String, ByVal lngL_CDate As Integer, _
                          ByRef strCTime As String, ByVal lngL_CTime As Integer, ByRef lngNVals As Integer, ByRef sngValues As Single, ByRef strCunits As String, _
                          ByVal lngL_CUnits As Integer, ByRef strCtype As String, ByVal lngL_CType As Integer, ByRef lngIplan As Integer, ByRef lngIstat As Integer)
            UnsafeNative.heclib.ZSRTS_(ifltab, strCPATH, strCDate, strCTime, lngNVals, sngValues, strCunits, strCtype, _
                                          lngIplan, lngIstat, lngL_CPath, lngL_CDate, lngL_CTime, lngL_CUnits, lngL_CType)
        End Sub

        Public Sub ZCOREC_(ByRef lngIFTOLD As Integer, ByRef lngIFTNEW As Integer, ByVal strCPOLD As String, ByVal lngL_CPOLD As Integer, _
                            ByVal strCPNEW As String, ByVal lngL_CPNEW As Integer, ByRef BUFF1() As Single, ByRef KBuff1 As Integer, _
                            ByRef BUFF2() As Single, ByRef KBuff2 As Integer, ByRef IStat As Integer)
            UnsafeNative.heclib.ZCOREC_(lngIFTOLD, lngIFTNEW, strCPOLD, strCPNEW, BUFF1(0), KBuff1, BUFF2(0), KBuff2, IStat, lngL_CPOLD, lngL_CPNEW)
        End Sub

    End Module
End Namespace