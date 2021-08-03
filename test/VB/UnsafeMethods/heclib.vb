Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Specialized
Imports System.Windows.Forms
Namespace UnsafeNative
    <System.Security.SuppressUnmanagedCodeSecurity()> _
    Public Module heclib
        Declare Function FORTRANOPEN_ Lib "HECLIB.dll" Alias "FORTRANOPEN_" (ByVal CNAME As String, ByRef IUnit As Integer, ByVal len_CNAME As Integer) As Integer
        Declare Function FORTRANCLOSE_ Lib "HECLIB.dll" Alias "FORTRANCLOSE_" (ByRef IUnit As Integer) As Integer
        Declare Sub ZCAT_ Lib "HECLIB.dll" Alias "ZCAT_" (ByRef lngIFLTAB As Integer, ByRef lngICUNIT As Integer, ByRef lngICDUNT As Integer, ByRef lngINUNIT As Integer,
                                                                ByVal strCINSTR As String, ByRef lngLABREV As Boolean, ByRef lngLDOSRT As Boolean, ByRef lngLCDCAT As Boolean,
                                                                ByRef lngNORECS As Integer, ByVal lngL_CINSTR As Integer)
        Declare Sub ZOPEN_ Lib "HECLIB.dll" Alias "ZOPEN_" (ByRef IFLTAB As Integer, ByVal CNAME As String, ByRef ISTAT As Integer, ByVal len_CNAME As Integer)
        Declare Sub ZCLOSE_ Lib "HECLIB.dll" Alias "ZCLOSE_" (ByRef IFLTAB As Integer)



        Declare Sub ZDELET_ Lib "HECLIB.dll" Alias "ZDELET_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef nPath As Integer,
                                                              ByRef LFound As Boolean, ByVal len_CPath As Integer)

        Declare Sub ZDTYPE_ Lib "HECLIB.dll" Alias "ZDTYPE_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef nSize As Integer, ByRef LExist As Boolean,
                                                              ByVal CDType() As Byte, ByRef DType As Integer, ByVal len_CPath As Integer, ByVal len_CDType As Integer)


        Declare Sub ZINQIR_ Lib "HECLIB.dll" Alias "ZINQIR_" (ByRef IFLTAB As Integer, ByVal CItem As String, ByVal CSTRING() As Byte, ByRef INUMB As Integer, _
                                                              ByVal len_CItem As Integer, ByVal len_CSTRING As Integer)


        Declare Sub ZRPD_ Lib "HECLIB.dll" Alias "ZRPD_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef NORD As Integer, ByRef NCURVE As Integer, ByRef Horiz As Integer, _
                                                ByVal C1Unit() As Byte, ByVal C1Type() As Byte, ByVal C2Unit() As Byte, ByVal C2Type() As Byte, ByRef Values As Single, _
                                                ByRef KVals As Integer, ByRef NVals As Integer, ByVal CLabel() As Byte, ByRef KLabel As Integer, ByRef Label As Boolean, _
                                                ByRef HEADU As Integer, ByRef HKeadU As Integer, ByRef NHEADU As Integer, ByRef ISTAT As Integer, ByVal len_CPath As Integer, _
                                                ByVal len_C1Unit As Integer, ByVal len_C1Type As Integer, ByVal len_C2Unit As Integer, ByVal len_C2Type As Integer, _
                                                ByVal len_CLabel As Integer)

        Declare Sub ZRRTS_ Lib "HECLIB.dll" Alias "ZRRTS_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByVal CDat As String, ByVal CTime As String, ByRef NVals As Integer, _
                                                               ByRef Values As Single, ByVal CUnits() As Byte, ByVal strCType() As Byte, ByRef IOFSET As Integer, ByRef ISTAT As Integer, _
                                                               ByVal len_CPath As Integer, ByVal len_CDate As Integer, ByVal len_CTime As Integer, ByVal len_CUnits As Integer, _
                                                               ByVal len_CType As Integer)

        Declare Sub ZSET_ Lib "HECLIB.dll" Alias "ZSET_" (ByVal CItem As String, ByVal CVal As String, ByRef num As Integer, ByVal len_CItem As Integer, _
                                                                  ByVal len_CVal As Integer)

        Declare Sub ZSPD_ Lib "HECLIB.dll" Alias "ZSPD_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef NORD As Integer, ByRef NCURVE As Integer, ByRef Horiz As Integer, _
                                                         ByVal C1Unit As String, ByVal C1Type As String, ByVal C2Unit As String, ByVal C2Type As String, ByRef Values As Single, _
                                                         ByVal CLabel As String, ByRef lngLabel As Boolean, ByRef HEADU As Integer, ByRef NHEADU As Integer, ByRef Plan As Integer, _
                                                         ByRef ISTAT As Integer, ByVal len_CPath As Integer, ByVal len_C1Unit As Integer, ByVal len_C1Type As Integer, _
                                                         ByVal len_C2Unit As Integer, ByVal len_C2Type As Integer, ByVal len_CLabel As Integer)

        Declare Sub ZSRTS_ Lib "HECLIB.dll" Alias "ZSRTS_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByVal CDat As String, ByVal CTime As String, ByRef NVals As Integer, _
                                                        ByRef SValues As Single, ByVal CUnits As String, ByVal CCType As String, ByRef IPLAN As Integer, ByRef ISTAT As Integer, _
                                                        ByVal len_CPath As Integer, ByVal len_CDat As Integer, ByVal len_CTime As Integer, ByVal len_CUnits As Integer, _
                                                        ByVal len_CType As Integer)

        Declare Sub ZCOREC_ Lib "HECLIB.dll" Alias "ZCOREC_" (ByRef IFTOLD As Integer, ByRef IFTNEW As Integer, ByVal CPOLD As String, ByVal CPNEW As String, ByRef BUFF1 As Single, _
                                                              ByRef KBuff1 As Integer, ByRef BUFF2 As Single, ByRef KBuff2 As Integer, ByRef ISTAT As Integer, ByVal len_CPOLD As Integer, _
                                                              ByVal len_CPNEW As Integer)

        Declare Sub SQUEEZEDSS_ Lib "HECLIB.dll" Alias "SQUEEZEDSS_" (ByVal CNAME As String, ByRef ISTAT As Integer, ByVal len_CNAME As Integer)
    End Module
End Namespace