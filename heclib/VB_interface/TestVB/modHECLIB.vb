Imports System.IO
Imports System
Imports System.Runtime.InteropServices
Imports System.Collections.Specialized
Imports System.Text
'Imports System.Runtime.InteropServices
'Imports System.Globalization 'GregorianCalendar class

#Region " Fortran DLL routines "
Namespace UnsafeNative
    <System.Security.SuppressUnmanagedCodeSecurity()>
    Public Module heclib
        Private Declare Sub ZCHKPN_ Lib "HECLIB.dll" Alias "ZCHKPN_" (ByVal strPath As String, ByRef nPath As Integer, ByRef ISTAT As Integer,
                                                                      ByVal lngL_CPath As Integer)
        Declare Function FORTRANOPEN_ Lib "HECLIB.dll" Alias "FORTRANOPEN_" (ByVal CNAME As String, ByRef IUnit As Integer, ByVal len_CNAME As Integer) As Integer
        Declare Function FORTRANCLOSE_ Lib "HECLIB.dll" Alias "FORTRANCLOSE_" (ByRef IUnit As Integer) As Integer
        Declare Sub ZCAT_ Lib "HECLIB.dll" Alias "ZCAT_" (ByRef lngIFLTAB As Integer, ByRef lngICUNIT As Integer, ByRef lngICDUNT As Integer, ByRef lngINUNIT As Integer,
                                                                ByVal strCINSTR As String, ByRef lngLABREV As Boolean, ByRef lngLDOSRT As Boolean, ByRef lngLCDCAT As Boolean,
                                                                ByRef lngNORECS As Integer, ByVal lngL_CINSTR As Integer)
        Declare Sub ZOPEN_ Lib "HECLIB.dll" Alias "ZOPEN_" (ByRef IFLTAB As Integer, ByVal CNAME As String, ByRef ISTAT As Integer, ByVal len_CNAME As Integer)
        Declare Sub ZCLOSE_ Lib "HECLIB.dll" Alias "ZCLOSE_" (ByRef IFLTAB As Integer)


        Declare Sub ZDTYPE_ Lib "HECLIB.dll" Alias "ZDTYPE_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef nSize As Integer, ByRef LExist As Boolean,
                                                                 ByVal CDType() As Byte, ByRef DType As Integer, ByVal len_CPath As Integer, ByVal len_CDType As Integer)

        Declare Sub ZOPNCA Lib "HECLIB.dll" Alias "ZOPNCA_" (ByVal CDSSFI As String, ByRef lngICUNIT As Integer, ByRef lngLGENCA As Boolean, ByRef lngLOPNCA As Boolean,
                                                                 ByRef lngLCATLG As Boolean, ByRef lngICDUNT As Integer, ByRef lngLGENCD As Boolean, ByRef lngLOPNCD As Boolean,
                                                                 ByRef lngLCATCD As Boolean, ByRef lngNRECS As Integer, ByVal len_CDSSFI As Integer)

        Declare Sub ZSET_ Lib "HECLIB.dll" Alias "ZSET_" (ByVal CItem As String, ByVal CVal As String, ByRef num As Integer, ByVal len_CItem As Integer,
                                                                 ByVal len_CVal As Integer)
        Declare Sub ZRPD_ Lib "HECLIB.dll" Alias "ZRPD_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef NORD As Integer, ByRef NCURVE As Integer, ByRef Horiz As Integer,
                                                             ByVal C1Unit() As Byte, ByVal C1Type() As Byte, ByVal C2Unit() As Byte, ByVal C2Type() As Byte, ByRef Values As Single,
                                                             ByRef KVals As Integer, ByRef NVals As Integer, ByVal CLabel() As Byte, ByRef KLabel As Integer, ByRef Label As Boolean,
                                                             ByRef HEADU As Integer, ByRef HKeadU As Integer, ByRef NHEADU As Integer, ByRef ISTAT As Integer, ByVal len_CPath As Integer,
                                                             ByVal len_C1Unit As Integer, ByVal len_C1Type As Integer, ByVal len_C2Unit As Integer, ByVal len_C2Type As Integer,
                                                             ByVal len_CLabel As Integer)

        Declare Sub ZSPD_ Lib "HECLIB.dll" Alias "ZSPD_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef NORD As Integer, ByRef NCURVE As Integer, ByRef Horiz As Integer,
                                                         ByVal C1Unit As String, ByVal C1Type As String, ByVal C2Unit As String, ByVal C2Type As String, ByRef Values As Single,
                                                         ByVal CLabel As String, ByRef lngLabel As Boolean, ByRef HEADU As Integer, ByRef NHEADU As Integer, ByRef Plan As Integer,
                                                         ByRef ISTAT As Integer, ByVal len_CPath As Integer, ByVal len_C1Unit As Integer, ByVal len_C1Type As Integer,
                                                         ByVal len_C2Unit As Integer, ByVal len_C2Type As Integer, ByVal len_CLabel As Integer)



        Declare Sub ZRRTS_ Lib "HECLIB.dll" Alias "ZRRTS_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByVal CDat As String, ByVal CTime As String, ByRef NVals As Integer,
                                                               ByRef Values As Single, ByVal CUnits() As Byte, ByVal strCType() As Byte, ByRef IOFSET As Integer, ByRef ISTAT As Integer,
                                                               ByVal len_CPath As Integer, ByVal len_CDate As Integer, ByVal len_CTime As Integer, ByVal len_CUnits As Integer,
                                                               ByVal len_CType As Integer)

        Declare Sub ZRRTSC_ Lib "HECLIB.dll" Alias "ZRRTSC_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByVal CDat As String, ByVal CTime As String, ByRef KVals As Integer,
                                                                     ByRef NVals As Integer, ByRef LGETDOB As Boolean, ByRef LFILDOB As Boolean, ByRef SValues As Single, ByRef DValues As Double,
                                                                     ByRef JQual As Integer, ByRef LQual As Boolean, ByRef LQREAD As Boolean, ByVal CUnits() As Byte, ByVal CCType() As Byte,
                                                                     ByVal CSUPP() As Byte, ByRef IOFSET As Integer, ByRef JCOMP As Integer, ByRef ITZONE As Integer, ByVal CTZONE() As Byte,
                                                                     ByRef COORDS As Double, ByRef ICDESC As Integer, ByRef LCOORDS As Integer, ByRef ISTAT As Integer, ByVal len_CPath As Integer,
                                                                     ByVal len_CDat As Integer, ByVal len_CTime As Integer, ByVal len_CUnits As Integer, ByVal len_CType As Integer,
                                                                     ByVal len_CSUPP As Integer, ByVal len_CCTZONE As Integer)


        Declare Sub ZSRTS_ Lib "HECLIB.dll" Alias "ZSRTS_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByVal CDat As String, ByVal CTime As String, ByRef NVals As Integer,
                                                        ByRef SValues As Single, ByVal CUnits As String, ByVal CCType As String, ByRef IPLAN As Integer, ByRef ISTAT As Integer,
                                                        ByVal len_CPath As Integer, ByVal len_CDat As Integer, ByVal len_CTime As Integer, ByVal len_CUnits As Integer,
                                                        ByVal len_CType As Integer)
        Declare Sub ZRITS_ Lib "HECLIB.dll" Alias "ZRITS_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef JULS As Integer, ByRef ISTIME As Integer, ByRef JULE As Integer,
                                                           ByRef IETIME As Integer, ByRef ITimes As Integer, ByRef Values As Single, ByRef KVals As Integer, ByRef NVals As Integer,
                                                           ByRef IBDate As Integer, ByVal CUnits() As Byte, ByVal CCType() As Byte, ByRef ISTAT As Integer, ByVal len_CPath As Integer,
                                                           ByVal len_CUnits As Integer, ByVal len_CType As Integer)

        Declare Sub ZSITS_ Lib "HECLIB.dll" Alias "ZSITS_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef ITimes As Integer, ByRef Values As Single, ByRef NVals As Integer,
                                                                   ByRef IBDate As Integer, ByVal CUnits As String, ByVal CCType As String, ByRef INFLAG As Integer, ByRef ISTAT As Integer,
                                                                   ByVal len_CPath As Integer, ByVal len_CUnits As Integer, ByVal len_CType As Integer)

        Declare Sub ZINQIR_ Lib "HECLIB.dll" Alias "ZINQIR_" (ByRef IFLTAB As Integer, ByVal CItem As String, ByVal CSTRING() As Byte, ByRef INUMB As Integer,
                                                      ByVal len_CItem As Integer, ByVal len_CSTRING As Integer)


        Declare Sub ZFNAME_ Lib "HECLIB.dll" Alias "ZFNAME_" (ByVal CIN As String, ByVal CNAME As String, ByRef NName As Integer, ByRef LExist As Integer, ByVal len_CIN As Integer,
                                                             ByVal len_CNAME As Integer)

        Declare Sub ZDELET_ Lib "HECLIB.dll" Alias "ZDELET_" (ByRef IFLTAB As Integer, ByVal CPATH As String, ByRef nPath As Integer,
                                                              ByRef LFound As Boolean, ByVal len_CPath As Integer)

        Declare Sub ZCOREC_ Lib "HECLIB.dll" Alias "ZCOREC_" (ByRef IFTOLD As Integer, ByRef IFTNEW As Integer, ByVal CPOLD As String, ByVal CPNEW As String, ByRef BUFF1 As Single,
                                                                     ByRef KBuff1 As Integer, ByRef BUFF2 As Single, ByRef KBuff2 As Integer, ByRef ISTAT As Integer, ByVal len_CPOLD As Integer,
                                                                     ByVal len_CPNEW As Integer)

        Declare Sub ZGPNP_ Lib "HECLIB.dll" Alias "ZGPNP_" (ByVal CLINE As String, ByVal LCLINE As Integer, ByVal CA As String, ByVal CB As String, ByVal CC As String, ByVal CD As String,
                                                                   ByVal CE As String, ByVal cf As String, ByRef NPARTS As Integer, ByVal LCA As Integer, ByVal LCB As Integer, ByVal LCC As Integer,
                                                                   ByVal LCD As Integer, ByVal LCE As Integer, ByVal LCF As Integer)

        Declare Sub SQUEEZEDSS_ Lib "HECLIB.dll" Alias "SQUEEZEDSS_" (ByVal CNAME As String, ByRef ISTAT As Integer, ByVal len_CNAME As Integer)

        Declare Sub DETTAXIS_ Lib "HECLIB.dll" Alias "DETTAXIS_" (ByRef JULS As Integer, ByRef ISTIME As Long, ByRef JULE As Long, ByRef IETIME As Integer, ByRef JMINTIC As Integer,
                                                                         ByRef ITMINTIC As Integer, ByRef NMINTIC As Integer, ByRef JMAJTIC As Integer, ByRef ITMAJTIC As Integer,
                                                                         ByRef NMAJTIC As Integer, ByRef J2AXIS As Integer, ByRef IT2AXIS As Integer, ByRef NC2AXISL As Integer,
                                                                         ByVal CoLMAJTIC As String, ByVal C2oAXISL As String, ByRef ISTAT As Integer, ByVal len_CoLMAJTIC As Integer,
                                                                   ByVal len_C2oAXISL As Integer)

        Declare Sub ZUPATH_ Lib "HECLIB.dll" Alias "ZUPATH_" (ByVal CLINE As String, ByRef IBPART As Integer, ByRef IEPART As Integer, ByRef ILPART As Integer, ByRef ISTAT As Integer, ByVal len_CLINE As Integer)

    End Module
End Namespace

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

        Public Sub ZRRTS_(ByRef ifltab As Integer, ByRef strCPATH As String, ByVal lngL_CPath As Integer,
                          ByRef strCDate As String, ByVal lngL_CDate As Integer, ByRef strCTime As String,
                          ByVal lngL_CTime As Integer, ByRef lngNVals As Integer, ByRef sngValues As Single, ByRef strCunits() As Byte,
                          ByVal lngL_CUnits As Integer, ByRef strCtype() As Byte, ByVal lngL_CType As Integer, ByRef lngIOFSET As Integer,
                          ByRef lngIstat As Integer)
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

        'Separate Path Parts
        Public Sub ZGPNP_(ByVal CLINE As String, ByVal LCLINE As Integer, _
            ByVal CA As String, ByVal LCA As Integer, ByVal CB As String, ByVal LCB As Integer, _
            ByVal CC As String, ByVal LCC As Integer, ByVal Cd As String, ByVal LCD As Integer, _
            ByVal CE As String, ByVal LCE As Integer, ByVal cf As String, ByVal LCF As Integer, _
            ByRef NPARTS As Integer)
            UnsafeNative.ZGPNP_(CLINE, LCLINE, CA, CB, CC, Cd, CE, cf, NPARTS, LCA, LCB, LCC, LCD, LCE, LCF)
        End Sub

        Public Sub ZUPATH_(ByVal strCpath As String, ByVal lngL_CPath As Integer, ByRef lngIBPART As Integer, _
                           ByRef lngIEPART As Integer, ByRef lngILPART As Integer, ByRef lngIstat As Integer)
            UnsafeNative.ZUPATH_(strCpath, lngIBPART, lngIEPART, lngILPART, lngIstat, strCpath.Length)
        End Sub

        Sub ZSITS_(ByRef lngIFLTAB As Integer, ByVal strCPATH As String, ByVal lngL_CPath As Integer, ByRef ITimes As Integer, ByRef Values As Single, _
        ByRef NVals As Integer, ByRef IBDate As Long, ByVal strCUnits As String, ByVal lngL_CUnits As Integer, ByVal strCType As String, ByVal lngL_CType As Integer, _
        ByRef INFlag As Integer, ByRef IStat As Integer)
            UnsafeNative.ZSITS_(lngIFLTAB, strCPATH, ITimes, Values, NVals, IBDate, strCUnits, strCType, INFlag, IStat, lngL_CPath, lngL_CUnits, lngL_CType)
        End Sub

        Public Sub ZRITS_(ByRef lngIFLTAB As Integer, ByVal strCPATH As String, ByVal lngL_CPath As Integer, ByRef JULS As Integer, ByRef ISTime As Integer, _
                          ByRef JULE As Integer, ByRef IETime As Integer, ByRef ITimes As Integer, ByRef Values As Single, ByRef KVals As Integer, _
                          ByRef NVals As Integer, ByRef IBDate As Long, ByVal bytCUnits() As Byte, ByVal lngL_CUnits As Integer, ByVal bytCType() As Byte, _
                          ByVal lngL_CType As Integer, ByRef IStat As Integer)
            UnsafeNative.ZRITS_(lngIFLTAB, strCPATH, JULS, ISTime, JULE, IETime, ITimes, Values, KVals, NVals, IBDate, bytCUnits, bytCType, IStat, lngL_CPath, lngL_CUnits, lngL_CType)
        End Sub
    End Module
End Namespace
#End Region


Module modHECLIB
    Public Const strWarning As String = "Illegal access to copyrighted program."
    Private mstrMessageFilename As String = ""


    'Fortran DLL routines
    'Public Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long

    <Runtime.InteropServices.DllImport("kernel32.dll", SetLastError:=True)> _
    Public Function LoadLibrary(ByVal lpFileName As String) As IntPtr
    End Function

    Declare Unicode Function GetShortPathName Lib "kernel32.dll" _
        Alias "GetShortPathNameW" (ByVal longPath As String, <MarshalAs(UnmanagedType.LPTStr)> _
        ByVal ShortPath As System.Text.StringBuilder, <MarshalAs(UnmanagedType.U4)> ByVal bufferSize As Integer) As Integer


    Public Cunit As Integer = 6
    Private CmsgUnit As Integer = 10

    Public Function FORTRANOPEN(ByVal strFilename As String, ByVal lngUnit As Integer) As Integer
        Return SafeNative.FORTRANOPEN_(strFilename, Len(strFilename), lngUnit)
    End Function

    Public Function FORTRANCLOSE(ByVal lngUnit As Integer) As Integer
        Return SafeNative.FORTRANCLOSE_(lngUnit)
    End Function

    Public Function hecSqueeze(ByVal strCPATH As String) As Long
        Dim lngIstat As Integer
        '    SQUEEZEDSS_ 6
        SafeNative.SQUEEZEDSS_(strCPATH, Len(strCPATH), lngIstat)

        ' TBD: Error handling
        '    If Not SuppressErrors Then
        '        If istat > 5 Then _
        '            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: severe error, code ", istat
        '        If istat <> 0 Then _
        '            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: some or all data missing"
        '        If nvals <> UBound(theData) - LBound(theData) + 1 Then _
        '            Err.Raise &H2001 Or vbObjectError, cstrMyName, "DSS: retrieved unexpected number of values"
        '    End If

        '    SQUEEZEDSS = 0
        Return lngIstat
    End Function


    ''' <summary>
    '''ZCOREC copies a record from one DSS file to another, or duplicates a record within the same file. A regular-interval time series record can also be compressed (Or un-compressed)
    ''' according to the New file's default data compression setting, if desired.
    ''' </summary>
    ''' <param name="iftold">The DSS work space used to manage the DSS file. This is the Output IFLTAB For the DSS file To copy from.</param>
    ''' <param name="addressOfIftnew">The DSS work space used to manage the DSS file. This is the Output IFLTAB For the DSS file To copy To. If the record Is To be duplicated within the same file, this should be array IFTOLD.</param>
    ''' <param name="cpold">Input The pathname of the record to copy.</param>
    ''' <param name="cpnew">Input The pathname that the copied record is to have. If the record is to be copied from one file to another, then this may be CPOLD.</param>
    ''' <returns></returns>
    Public Function hecZcorec(ByRef iftold() As Integer, ByVal addressOfIftnew() As Integer, ByVal cpold As String, ByVal cpnew As String) As Integer
        Const kbuff1 As Integer = 8192
        Const kbuff2 As Integer = 100
        Dim intRet As Integer
        Dim buff1(kbuff1) As Single
        Dim buff2(kbuff2) As Single
        SafeNative.ZCOREC_(iftold(0), addressOfIftnew(0), cpold, Len(cpold), cpnew, Len(cpnew), buff1, kbuff1, buff2, kbuff2, intRet)
        Return intRet
    End Function

    'Public Function hecZundel(ByRef lngIfltab() As Integer, ByVal Pathname As String) As Integer
    '    ZUNDEL(lngIfltab(1), Pathname, Len(Pathname), Len(Pathname), hecZundel)
    'End Function


    ''' <summary>
    ''' ZDELET deletes a record from a DSS file by flagging a record status cell. The data is
    ''' Not physically removed until the file Is squeezed by DSSUTL. A deleted record can be
    ''' undeleted by DSSUTL Or the subroutines ZUNDEL And ZUDALL (until the file Is squeezed).
    ''' </summary>
    ''' <param name="lngIfltab">The DSS work space used to manage the DSS file. This is the Output same array used In the ZOPEN Call.</param>
    ''' <param name="Pathname">The pathname of the record to eliminate. This pathname does Not have to follow the standard conventions.</param>
    ''' <returns></returns>
    Public Function hecZdelet(ByRef lngIfltab() As Integer, ByVal Pathname As String) As Integer
        Dim intret As Integer
        SafeNative.ZDELET_(lngIfltab(0), Pathname, Len(Pathname), Len(Pathname), intret)
        Return intret
    End Function

    'Public Function hecZrenam(ByRef lngIfltab() As Integer, ByRef strOldPathname As String, ByRef strNewPathname As String) As Integer
    '    ZRENAM(lngIfltab(1), strOldPathname, Len(strOldPathname), Len(strOldPathname), strNewPathname, Len(strNewPathname), Len(strNewPathname), hecZrenam)
    'End Function



    'Public Function hecZfver(ByVal Cname As String, ByRef istat As Integer) As String
    '    Dim strhecZfver As String = ""
    '    Dim strCver As String = "".PadRight(4)
    '    ZFVER(Cname, Len(Cname), strCver, Len(strCver), istat)
    '    If istat > 0 Then hecZfver = RTrim(strCver)
    '    Return strhecZfver
    'End Function

    ''' <summary>
    ''' This routine determines a DSS paths data type
    '''        lngNsize - Length of record
    ''' 
    '''       DType   CDType
    '''          0     UND    - Undefined type or could not find path
    '''        100     RTS    - Regular-Interval Time Series Data
    '''        110     ITS    - Irregular-Interval Time Series Data
    '''        200     PD     - Paired Data
    '''        300     TXT    - Text Data
    ''' </summary>
    ''' <param name="lngIfltab">FTab</param>
    ''' <param name="Pathname">the path to the record in the .dss file</param>
    ''' <param name="Exists">Record exists</param>
    ''' <param name="Datatype">the dataType (output)</param>
    ''' <param name="lngNsize">the size of the record (output)</param>
    Public Sub hecZdtype(ByRef lngIfltab() As Integer, ByVal Pathname As String, ByRef Exists As Boolean,
                         ByRef Datatype As String, ByRef lngIDType As Integer, ByRef lngNsize As Integer)

        Dim DTypeBA() As Byte = New Byte((3) - 1) {}

        Dim strPath As String = Pathname.PadRight(392)
        SafeNative.ZDTYPE_(lngIfltab(0), strPath, Len(strPath), lngNsize, Exists, DTypeBA, Len(Datatype), lngIDType)
        Dim asciiEncoder As System.Text.Encoding = New System.Text.ASCIIEncoding
        '   convert dataType from byte array to string
        Datatype = asciiEncoder.GetString(DTypeBA).Trim
    End Sub

    Public Function hecDatJul(ByVal strDate As String) As Long
        Dim IERROR As Integer
        Dim julian As Long
        DATJUL(CDate(strDate), julian, IERROR)

        Return julian
    End Function


    ''' <remarks>
    ''' 0  June 2, 1985  10  June 2, 85  100  JUNE 2, 1985  110  JUNE 2, 85
    ''' 1  Jun 2, 1985   11  Jun 2, 85   101  JUN 2, 1985   111  JUN 2, 85
    ''' 2  2 June 1985   12  2 June 85   102  2 JUNE 1985   112  2 JUNE 85
    ''' 3  June 1985     13  June 85     103  JUNE 1985     113  JUNE 85
    ''' 4  02Jun1985     14  02Jun85     104  02JUN1985     114  02JUN85
    ''' 5  2Jun1985      15  2Jun85      105  2JUN1985      115  2JUN85
    ''' 6  Jun1985       16  Jun85       106  JUN1985       116  JUN85
    ''' 7  02 Jun 1985   17  02 Jun 85   107  02 JUN 1985   117  02 JUN 85
    ''' 8  2 Jun 1985    18  2 Jun 85    108  2 JUN 1985    118  2 JUN 85
    ''' 9  Jun 1985      19  Jun 85      109  JUN 1985      119  JUN 85
    ''' </remarks>
    Public Function hecJulDat(ByVal juldate As Integer, Optional ByVal iStyle As Integer = 4) As String
        Dim CCDate As String = ""
        Dim NDate As Integer

        ' Console.WriteLine("hecJulDat   1 - juldate: " & juldate)

        JULDAT(juldate, iStyle, CCDate, NDate)
        Return Trim(Left(CCDate, NDate))
    End Function

    Public Sub hecDatCll(ByVal jbdate As Integer, ByVal itime As Integer, ByRef juldate As Integer, ByRef mins As Integer)
        DATCLL(jbdate, itime, juldate, mins)
    End Sub

    Private Sub AttachMessageFile()
        Dim strMsg As String = ""
        Dim strTempFolder As New System.Text.StringBuilder(255)
        Dim lngStatus As Integer
        Dim intTrial As Integer = 0
        Const MAX_INST As Integer = 20 ' max number of simultaneous instances; this is an arbitrary number
        Dim iLen As Integer
        Dim strBlank As String = ""
        Dim strmUnit As String = "MUNIT"
        'Routine to establish DSS message levels and output file
        'Should only be called once at the beginning of operations for this instance of the server
        Try
            strMsg = " padding temp folder variable with 255 spaces"
            'strTempFolder.Append(Space(255))
            strMsg = " getting temp folder length"
            iLen = strTempFolder.Length
            strMsg = " getting temp folder pathname"
            strTempFolder.Append(IO.Path.GetTempPath())
            strMsg = " getting temp folder short pathname"

            If GetShortPathName(IO.Path.GetTempPath(), strTempFolder, iLen) = 0 Then
                Throw New Exception("Failed to return a short path")
            End If
            'GetTempPath(Len(strTempFolder), strTempFolder)
            Dim strTempFolder2 As String = Trim(strTempFolder.ToString) 'Left(strTempFolder.ToString, InStr(1, strTempFolder.ToString, Chr(0)) - 1)

            If Len(mstrMessageFilename) = 0 Then
                Do
                    mstrMessageFilename = IO.Path.Combine(strTempFolder2, "~HECDSS_messages_NET_" & CStr(intTrial) & ".txt")
                    'opendssoutput_ mstrMessageFilename, Len(mstrMessageFilename), lngStatus

                    strMsg = " opening message file " & mstrMessageFilename
                    lngStatus = FORTRANOPEN(mstrMessageFilename, CmsgUnit)
                    strMsg = " setting message unit " & CmsgUnit
                    SafeNative.ZSET_(strmUnit, strmUnit.Length, strBlank, strBlank.Length, CmsgUnit)
                    intTrial = intTrial + 1
                Loop Until lngStatus = 0 Or intTrial >= MAX_INST
                'If intTrial exceeds MAX_INST, the file won't be opened, but instead it will attempt to write to Fort.69
            End If
        Catch ex As Exception
            Dim ex2 As New Exception(String.Format("AttachMessageFile: {0} when {1}.", ex.Message, strMsg))
            Throw ex2
        End Try
    End Sub


    ' Opens the message file, then the DSS file
    Function hecZopen(ByRef ifltab() As Integer, ByRef DSSFileName As String, ByRef MsgLvl As Integer) As Integer
        Dim istat As Integer
        Dim strMessage As String = ""
        Dim iret As Integer
        Try
            strMessage = " attaching message file"
            AttachMessageFile()
            strMessage = " setting message level" & MsgLvl
            hecZset("MLEV", "", MsgLvl)

            DSSFileName = Trim(DSSFileName)

            strMessage = " calling ZOPEN for " & DSSFileName
            SafeNative.ZOPEN_(ifltab(0), DSSFileName, DSSFileName.Length, istat)
            iret = istat
            Return iret
        Catch ex As Exception
            Throw New Exception(String.Format("hecZopen: {0} while {1}. ", ex.Message, strMessage))
        End Try

    End Function

    Sub hecZclose(ByRef ifltab() As Integer)
        SafeNative.ZCLOSE_(ifltab(0))
    End Sub

    Sub hecMakeDSScatalog(ByRef ifltab() As Integer, ByRef DSSFileName As String, ByRef MsgLvl As Integer)
        Dim lngErrorCode As Long
        Dim lngNoRecs As Integer
        Dim strDssTemp As String

        FORTRANCLOSE(Cunit)
        strDssTemp = Replace(DSSFileName, ".dss", ".dsc", , , CompareMethod.Text)
        lngErrorCode = FORTRANOPEN(strDssTemp, Cunit)
        If lngErrorCode <> 0 Then Err.Raise(&H4000 + lngErrorCode)
        SafeNative.ZCAT_(ifltab(0), Cunit, 0, 0, " ", 1, 1, 0, 0, lngNoRecs)
        If lngErrorCode <> 0 Then Err.Raise(&H4000 + lngErrorCode)
        FORTRANCLOSE(Cunit)
        'hecCloseMessageFile()
    End Sub

    Sub hecZsrts(ByRef ifltab() As Integer, ByRef path As String, ByRef StartDate As String, ByRef StartTime As String,
                 ByRef theData() As Single, ByRef theUnits As String, ByRef theType As String, ByRef desiredPlan As Integer, ByRef ioStatus As Integer)
        Dim nvals, iplan As Integer
        Dim theCDate As String = StartDate
        Dim theCTime As String = StartTime
        iplan = desiredPlan
        Dim theCUnits As String = theUnits
        Dim theCType As String = theType

        nvals = UBound(theData, 1)
        ioStatus = 0
        SafeNative.ZSRTS_(ifltab(0), path, path.Length, theCDate, theCDate.Length, theCTime, theCTime.Length, nvals,
                          theData(1), theCUnits, theCUnits.Length, theCType, theCType.Length, iplan, ioStatus)
        'Call ZSRTS(ifltab(1), fixedPathName, Len(fixedPathName), theCDate, Len(theCDate), theCTime, Len(theCTime), nvals, theData(1), theCUnits, Len(theCUnits), theCType, Len(theCType), iplan, ioStatus)
    End Sub

    ''' <summary>
    ''' ZRRTS is a short call to retrieve regular-interval time series data from a DSS file. The
    ''' data retrieved may be based On a time window And can cross record boundaries (that Is, it can
    ''' read several records With different dates To retrieve the data specified), Or it can read all the data
    ''' in one record according to the pathname (with no time window). When reading data based on a
    ''' time window, the D part Is ignored, As ZRRTS forms pathnames With a D part determined by
    ''' that time window. The time window Is specified by variables CDate, CTIME, And NVALS. If
    ''' data flags, compression information, Or the user header needs To be retrieved, use subroutine
    ''' ZRRSTX, the extended version of this subroutine.
    ''' </summary>
    ''' <param name="ifltab">The DSS work space used to manage the DSS file. This is the Output same array used In the ZOPEN Call</param>
    ''' <param name="path">The pathname of the data to read. The pathname must meet the regular-interval time series conventions (including a correct "E
    ''' part"). With a time window specified, the "D part" (date part) will be ignored; As ZRRTS will form it internally (there may be
    ''' several D parts, depending On the time window). If no time window Is given, the D part must be provided. The length of CPATH Is implicit (e.g., CPATH(1:NPATH)).</param>
    ''' <param name="StartDate">The beginning date of the time window. This can be in anyone of the styles accepted by the HECLIB subroutine DATJUL
    ''' (see the HECLIB documentation for the different date styles).  If the data Is To be retrieved based upon the Date In the 
    ''' pathname (that Is, no time window), CDate should be blank (i.e., ' ').</param>
    ''' <param name="StartTime">The beginning time of the time window. This must be a standard twenty-four hour clock time (e.g., '1630'). If no time
    ''' window Is set (CDate Is blank), this argument Is ignored.</param>
    ''' <param name="theData">The data retrieved. This will be in a sequential order, with the first value having a Date And time Of CDate And CTIME
    ''' (unless no time window Is given, whereas the first value will correspond to the date And time of the beginning of the record).</param>
    ''' <param name="theUnits">The units of the data (e.g., 'FEET').</param>
    ''' <param name="theType">The type of the data (e.g., 'PER-AVER').</param>
    ''' <param name="intOffset">The time offset of the data in minutes. (If hourly data is recorded at fifteen minutes past the hour, the offset would be
    ''' fifteen minutes.) If there Is no offset, IOFSET will be returned as zero. Refer to the subroutine ZOFSET (at the end of this chapter) for more 
    ''' information about time offsets. The offset must be Integer*4 On MS DOS microcomputers.</param>
    ''' <param name="ioStatus"></param>
    ''' <returns>A status parameter indicating the success of the operation. If ISTAT Is returned as zero, then the data was successfully read.
    ''' If ISTAT Is returned With a value between one And three, Then data was retrieved, but some missing values were detected. If
    ''' ISTAT Is greater than ten, a fatal error occurred, And no data was returned.</returns>
    Function hecZrrts(ByRef ifltab() As Integer, ByRef path As String, ByRef StartDate As String, ByRef StartTime As String, ByRef theData() As Single, ByRef theUnits As String, ByRef theType As String, ByRef intOffset As Integer, ByRef ioStatus As Integer) As Object
        ' Returns number of values actually read
        Dim asciiEncoder As System.Text.Encoding = New System.Text.ASCIIEncoding
        Dim nvals, iOfset As Integer
        Dim theCDate As String = StartDate
        Dim theCTime As String = StartTime
        'Dim theCUnits As String = "".PadRight(8)
        'Dim theCType As String = "".PadRight(8)

        Dim theCUnits() As Byte = New Byte((8) - 1) {}
        Dim theCType() As Byte = New Byte((8) - 1) {}

        nvals = UBound(theData, 1)
        ioStatus = 0
        iOfset = 0

        SafeNative.ZRRTS_(ifltab(0), path, path.Length, theCDate, theCDate.Length, theCTime, theCTime.Length, nvals, _
                          theData(1), theCUnits, theCUnits.Length, theCType, theCType.Length, iOfset, ioStatus)
        Dim converter As String = asciiEncoder.GetString(theCUnits)
        intOffset = iOfset
        theUnits = converter.Trim
        converter = asciiEncoder.GetString(theCType)
        theType = converter.Trim
        Return nvals
    End Function

    Sub hecZpath(ByRef Apart As String, ByRef Bpart As String, ByRef Cpart As String, ByRef Dpart As String, ByRef Epart As String, ByRef Fpart As String, ByRef path As String)
        Dim c(6) As String
        Dim cpath As String
        c(1) = Apart
        c(2) = Bpart
        c(3) = Cpart
        c(4) = Dpart
        c(5) = Epart
        c(6) = Fpart

        cpath = String.Format("/{0}/{1}/{2}/{3}/{4}/{5}/", Trim(c(1)), Trim(c(2)), Trim(c(3)), Trim(c(4)), Trim(c(5)), Trim(c(6)))
        path = (Trim(cpath))

        'ZPATH no logger available under current heclid60.
        'Dim c(6) As String
        'Dim npath As Integer
        'Dim mstrAPart As String = Apart.PadRight(32)
        'Dim mstrBPart As String = Bpart.PadRight(32)
        'Dim mstrCPart As String = Cpart.PadRight(32)
        'Dim mstrDPart As String = Dpart.PadRight(32)
        'Dim mstrEPart As String = Epart.PadRight(32)
        'Dim mstrFPart As String = Fpart.PadRight(32)
        'Dim cpath As String = "".PadRight(80)
        'Call ZPATH(mstrAPart, mstrAPart.Length, mstrBPart, mstrBPart.Length, mstrCPart, mstrCPart.Length, mstrDPart, mstrDPart.Length, mstrEPart, mstrEPart.Length, mstrFPart, mstrFPart.Length, cpath, cpath.Length, npath)
        'path = Left(cpath, npath)
    End Sub

    Private Function FixedLength(ByVal S As String, ByVal IntLength As Integer) As String
        Dim l As Long
        FixedLength = ""
        l = Len(S)
        If l > IntLength Then
            FixedLength = Microsoft.VisualBasic.Left(S, IntLength)
        ElseIf l < IntLength Then
            FixedLength = S.PadRight(IntLength)
        End If
        Return FixedLength
    End Function

    ''' <summary>
    ''' ZUPATH determines the beginning and ending position, and length of each part of a
    ''' pathname. This information Is returned in three six-element integer arrays. The subroutine
    ''' ZUFPN may be called instead Of ZUPATH To Return the pathname parts.
    ''' </summary>
    ''' <param name="path">The pathname to process.</param>
    ''' <param name="Apart">Part A</param>
    ''' <param name="Bpart">Part B</param>
    ''' <param name="Cpart">Part C</param>
    ''' <param name="Dpart">Part D</param>
    ''' <param name="Epart">Part E</param>
    ''' <param name="Fpart">Part F</param>
    Sub hecZupath(ByRef path As String, ByRef Apart As String, ByRef Bpart As String, ByRef Cpart As String, ByRef Dpart As String, ByRef Epart As String, ByRef Fpart As String)
        Dim lngBPart(6) As Integer
        Dim lngEPart(6) As Integer
        Dim lngLPart(6) As Integer
        Dim istat As Integer
        Try
            Dim cpath As String = UCase(Left(path, 392))
            SafeNative.ZUPATH_(cpath, Len(cpath), lngBPart(1), lngEPart(1), lngLPart(1), istat)
            If istat = 0 Then
                Apart = Mid(cpath, lngBPart(1), lngLPart(1))
                Bpart = Mid(cpath, lngBPart(2), lngLPart(2))
                Cpart = Mid(cpath, lngBPart(3), lngLPart(3))
                Dpart = Mid(cpath, lngBPart(4), lngLPart(4))
                Epart = Mid(cpath, lngBPart(5), lngLPart(5))
                Fpart = Mid(cpath, lngBPart(6), lngLPart(6))
            Else
                Apart = ""
                Bpart = ""
                Cpart = ""
                Dpart = ""
                Epart = ""
                Fpart = ""
                Throw New Exception("Error " & "DSSErrors.dssBadPathname" &
                    " in hecZupath. Bad pathname: " & cpath)
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub

    'Not available in version 6
    'Sub hecZchkpn(ByRef strPath As String, ByRef lngIstat As Integer)
    '    Dim npath As Integer
    '    Dim cpath As String = strPath.PadRight(80)
    '    npath = Len(strPath)
    '    Call ZCHKPN(cpath, Len(cpath), npath, lngIstat)
    'End Sub


    ''' <summary>
    ''' ZRITS is a short call to retrieve irregular-interval time series data from a DSS file. The 
    ''' data retrieved may be based On a time window And can cross record boundaries (that Is, it can read
    ''' several records With different dates To retrieve the data specified), Or it can read all the data In one
    ''' record according To the pathname (With no time window). When reading data based On a time
    ''' window, the D part Is ignored, as ZRITS forms pathnames with a D part determined by the time window.
    ''' </summary>
    ''' <param name="ifltab">The DSS workspace used to manage the DSS file. This is the Output same array used In the ZOPEN Call.</param>
    ''' <param name="path">The pathname of the data to read. The pathname must meet the irregular-interval time series conventions (including a correct E
    ''' part). With a time window specified, the D part (date part) will be ignored, as ZRITS will form it internally (there may be
    ''' several D parts, depending On the time window). If no time window Is given, the D part must be provided. The length of
    ''' CPATH Is implicit (e.g., CPATH(1:NPATH)).</param>
    ''' <param name="juls">The Julian date of the start of the time window. This is days since December 31, 1899, Not since the beginning Of the current
    ''' year. If no time window Is specified, this argument Is ignored (see ISTIME).</param>
    ''' <param name="istime">The starting time of the time window, in minutes past midnight (for midnight ISTIME would be 1440, Not zero). To read the
    ''' entire record(with no time window set), set ISTIME To -2. The D part Of the pathname will be used To define the time window.</param>
    ''' <param name="jule">The Julian date of the end of the time window in days since December 31, 1899. If no time window Is set, this argument Is
    ''' ignored.</param>
    ''' <param name="ietime">The ending time of the time window in minutes past midnight. If no time window Is Set, this argument Is ignored.</param>
    ''' <param name="itimes">An array containing the relative date/times of the data values, in a one-To-one correspondence. The times are given In minutes
    ''' since the base Date (JBDATE), And can be converted into Julian dates And times using the procedure described in the remarks section.</param>
    ''' <param name="VALUES">The values retrieved. The date/time of each value is provided in array ITIMES. Both arrays VALUES And ITIMES must be
    ''' dimensioned to KVALS.</param>
    ''' <param name="nvals">The number of values retrieved. Arrays ITIMES and VALUES will contain NVALS elements.</param>
    ''' <param name="jbdate">The Julian base date (in days since Dec. 31, 1899), usually equivalent to the D part of the first pathname. This date, in
    ''' conjunction with the ITIMES array, gives the date/time of each data value.</param>
    ''' <param name="cunits">The units of the data (e.g., 'FEET').</param>
    ''' <param name="ctype_Renamed">The type of the data (e.g., 'PER-AVER').</param>
    ''' <param name="istat">A status parameter indicating the success of the operation. If ISTAT Is returned with zero, then the data was successfully</param>
    Sub hecZrits(ByRef ifltab() As Integer, ByRef path As String, ByRef juls As Integer, ByRef istime As Integer, ByRef jule As Integer, ByRef ietime As Integer, ByRef itimes() As Integer, ByRef VALUES() As Single, ByRef nvals As Integer, ByRef jbdate As Integer, ByRef cunits As String, ByRef ctype_Renamed As String, ByRef istat As Integer)
        Dim asciiEncoder As System.Text.Encoding = New System.Text.ASCIIEncoding
        Dim YTypeBA() As Byte = New Byte((8) - 1) {}
        Dim YUnitsBA() As Byte = New Byte((8) - 1) {}

        Dim kvalues As Integer
        kvalues = UBound(itimes)
        SafeNative.ZRITS_(ifltab(0), path, path.Length, juls, istime, jule, ietime, itimes(1), VALUES(1), kvalues, nvals, jbdate, _
        YUnitsBA, YUnitsBA.Length, YTypeBA, YTypeBA.Length, istat)

        ctype_Renamed = asciiEncoder.GetString(YTypeBA).Trim
        cunits = asciiEncoder.GetString(YUnitsBA).Trim
    End Sub

    ''' <summary>
    ''' ZRITS is a short call to retrieve irregular-interval time series data from a DSS file. The
    ''' data retrieved may be based On a time window And can cross record boundaries (that Is, it can read
    ''' several records With different dates To retrieve the data specified), Or it can read all the data In one
    ''' record according To the pathname (With no time window). When reading data based On a time
    ''' window, the D part Is ignored, as ZRITS forms pathnames with a D part determined by the time window.
    ''' </summary>
    ''' <param name="ifltab">The DSS workspace used to manage the DSS file. This is the Output same array used In the ZOPEN Call.</param>
    ''' <param name="path">The pathname of the data to read. The pathname must meet the irregular-interval time series conventions (including a correct E
    ''' part). With a time window specified, the D part (date part) will be ignored, as ZRITS will form it internally (there may be
    ''' several D parts, depending On the time window). If no time window Is given, the D part must be provided. The length of
    ''' CPATH Is implicit (e.g., CPATH(1:NPATH)).</param>
    ''' <param name="itimes">An array containing the relative date/times of the data values, in a one-To-one correspondence. The times are given In minutes
    ''' since the base Date (JBDATE), And can be converted into Julian dates And times using the procedure described in the remarks section.</param>
    ''' <param name="VALUES">The values retrieved. The date/time of each value is provided in array ITIMES. Both arrays VALUES And ITIMES must be
    ''' dimensioned to KVALS.</param>
    ''' <param name="jbdate">The Julian base date (in days since Dec. 31, 1899), usually equivalent to the D part of the first pathname. This date, in
    ''' conjunction with the ITIMES array, gives the date/time of each data value.</param>
    ''' <param name="cunits">The units of the data (e.g., 'FEET').</param>
    ''' <param name="ctype_Renamed">The type of the data (e.g., 'PER-AVER').</param>
    ''' <param name="InFlag">INFLAG is a flag to indicate whether the data should be replaced Or merged with existing data. Replace will replace all
    ''' the data between the implied time window (time Of first And last data). Merge will combine the data with the data already stored.
    ''' (Merging data replaces data occurring at the same time And inserts data at New times.)
    '''     INFLAG = 0 to merge data.
    '''     INFLAG = 1 to replace data.</param>
    ''' <param name="istat">A status parameter indicating the success of the operation. If ISTAT Is returned with zero, then all the data was successfully
    ''' stored. If ISTAT Is greater than ten, a fatal error occurred.</param>
    Sub hecZsits(ByRef ifltab() As Integer, ByRef path As String, ByRef itimes() As Integer, ByRef VALUES() As Single, _
        ByRef jbdate As Integer, ByRef cunits As String, ByRef ctype_Renamed As String, ByRef InFlag As Integer, _
        ByRef istat As Integer)
        Dim lngNvalues As Integer

        lngNvalues = UBound(VALUES) + 1
        SafeNative.ZSITS_(ifltab(0), path, path.Length, itimes(0), VALUES(0), lngNvalues, jbdate, cunits, cunits.Length,
            ctype_Renamed, ctype_Renamed.Length, InFlag, istat)


    End Sub

    ''' <summary>
    ''' ZSPD stores paired (curve) data in a DSS file. Curve labels and the user header may be stored in addition to the data.
    ''' </summary>
    ''' <param name="lngIfltab">The DSS work space used to manage the DSS file. This is the Output same array used In the ZOPEN Call.</param>
    ''' <param name="strCpath">The pathname of the data to store. The pathname must meet the paired Function data conventions.</param>
    ''' <param name="lngNord">The number of ordinates (number of points per curve). Each curve to be stored in a single record must have the same
    ''' number of ordinates.</param>
    ''' <param name="lngNcurve">The number of curves to store in this record.</param>
    ''' <param name="lngIHORIZ">The variable number to appear on the horizontal axis for plotting (one for first variable, two for second).</param>
    ''' <param name="strC1unit">The units of the first variable (e.g., 'FEET', 'PERCENT').</param>
    ''' <param name="strC1type">The type of data for the first variable. The following types are recognized by DSS utility programs:
    '''     UNT Untransformed
    '''     LOG Logarithmic - data expressed As logarithms.
    '''     PROB Probability - data expressed In percent.
    ''' </param>
    ''' <param name="strC2unit">The units of the second variable.</param>
    ''' <param name="strC2type">The type of data for the second variable.</param>
    ''' <param name="sngValues">The data values to store. The first NORD elements in VALUES correspond To the first variable (the X axis). The
    ''' data for the second variable must begin at element NORD+1 (the Y axis). Y axis values for a second curve would begin at (NORD * 2) + 1.
    ''' </param>
    ''' <param name="strClabel">A optional character array with labels corresponding to each curve. For example, if an ELEVATION-DAMAGE function Is
    ''' to be stored containing residential, agricultural And commercial damage, then CLABEL might be as follows:
    '''     CLABEL(1) = 'RESIDENTIAL '
    '''     CLABEL(2) = 'AGRICULTURAL'
    '''     CLABEL(3) = 'COMMERCIAL '
    ''' For this example, NCURVE would be returned with three, and CLABEL should be dimensioned To at least three.
    ''' </param>
    ''' <param name="lngIplan">A flag indicating whether to write over existing data or not:
    '''     IPLAN           Description
    '''     0       Always write the record to the file.
    '''     1       Only write the record if it Is New (i.e., no record previously existed In that file under that pathname).
    '''     2       Only write the data if the record already existed in the file.
    ''' </param>
    ''' <param name="lngIstat">A status parameter indicating the success of the operation. If ISTAT Is returned with zero, then the data was successfully
    ''' stored, otherwise an error occurred.
    ''' </param>
    Sub hecZspd(ByRef lngIfltab() As Integer, ByRef strCpath As String, ByRef lngNord As Integer, ByRef lngNcurve As Integer, _
    ByRef lngIHORIZ As Integer, ByRef strC1unit As String, ByRef strC1type As String, ByRef strC2unit As String, _
    ByRef strC2type As String, ByRef sngValues() As Single, ByRef strClabel() As String, ByRef lngIplan As Integer, _
    ByRef lngIstat As Integer)
        Dim strLabels() As String
        Dim i As Short
        Dim lngLabelsPresent As Integer

        If UBound(strClabel) >= 1 Then
            ReDim strLabels(UBound(strClabel))
            For i = 0 To lngNcurve
                If IsNothing(strClabel(i)) Then
                    strClabel(i) = ""
                End If
                strLabels(i) = FixedLength(strClabel(i), 12)
            Next
            lngLabelsPresent = 1
        Else
            ReDim strLabels(1)
            strLabels(0) = ""
            strLabels(1) = ""
        End If
        SafeNative.ZSPD_(lngIfltab(0), strCpath, Len(strCpath), lngNord, lngNcurve, lngIHORIZ, strC1unit, Len(strC1unit), strC1type, _
            Len(strC1type), strC2unit, Len(strC2unit), strC2type, Len(strC2type), sngValues(1), strLabels(1), _
            Len(strLabels(1)) * UBound(strLabels), lngLabelsPresent, 0, 0, lngIplan, lngIstat)
    End Sub


    ''' <summary>
    ''' ZRPD retrieves paired (curve) data from a DSS file. The curve's labels and the user header may be retrieved In addition To the data.
    ''' </summary>
    ''' <param name="ifltab">The DSS work space used to manage the DSS file. This is the Output same array used In the ZOPEN Call.</param>
    ''' <param name="cpath">The pathname of the data to read. The pathname must meet the paired Function data() conventions.</param>
    ''' <param name="lngNord">The number of ordinates (number of points per curve) read. Each curve within a single record will have the same number
    ''' of ordinates.</param>
    ''' <param name="lngNcurve">The number of curves retrieved in this record.</param>
    ''' <param name="lngIHORIZ">The variable number to appear on the horizontal axis for plotting (one for first variable, two for second).</param>
    ''' <param name="Units">The units of the first variable (e.g., 'FEET', 'PERCENT').</param>
    ''' <param name="Datatype">The type of data for the first variable. The following types are
    ''' recognized by DSS utility programs:
    '''      UNT Untransformed
    '''      LOG Logarithmic - data expressed As logarithms.
    '''      PROB Probability - data expressed In percent.</param>
    ''' <param name="VALUES">The data values retrieved. The first NORD elements in VALUES correspond To the first variable (i.e., the X axis
    ''' values of the data points). The data for the second variable (theY axis values) begins at element NORD+1. Y axis values For a
    ''' second curve would begin at (NORD * 2) + 1.</param>
    ''' <param name="CLABEL">The labels for each curve. For example, if an ELEVATION-DAMAGE function Is retrieved containing 
    ''' residential, agricultural And commercial damage, then CLABEL might be returned As:
    '''     CLABEL(1) = 'RESIDENTIAL '
    '''     CLABEL(2) = 'AGRICULTURAL'
    '''     CLABEL (3) = 'COMMERCIAL 
    '''     For this example, NCURVE would be returned with three, And CLABEL should be dimensioned To at least three.</param>
    ''' <param name="LABEL">A logical variable indicating if labels were returned. LABEL will be Set To .True. If labels were retrieved (there will be
    ''' NCURVE labels), otherwise it will be As .False.</param>
    ''' <param name="ioStatus">A status parameter indicating the success of the operation. If ISTAT Is returned with zero, then the data was successfully
    ''' read.</param>
    Sub hecZrpd(ByRef ifltab() As Integer, ByRef cpath As String, ByRef lngNord As Integer, ByRef lngNcurve As Integer, _
    ByRef lngIHORIZ As Integer, ByRef Units() As String, ByRef Datatype() As String, ByRef VALUES() As Single, _
    ByRef CLABEL() As String, ByRef LABEL As Boolean, ByRef ioStatus As Integer)
        Dim asciiEncoder As System.Text.Encoding = New System.Text.ASCIIEncoding
        Const KLABEL As Short = 50
        Dim lngNheadu, lngLabel As Integer
        Dim lngNvals As Integer
        Dim strClabel As String
        Dim i As Short
        Dim lngDummy As Integer

        '   Read in the Paired Data
        Dim xUnitsBA() As Byte = New Byte((8) - 1) {}
        Dim yUnitsBA() As Byte = New Byte((8) - 1) {}
        Dim xTypeBA() As Byte = New Byte((8) - 1) {}
        Dim yTypeBA() As Byte = New Byte((8) - 1) {}
        Dim labelBA() As Byte = New Byte((30) - 1) {}

        'Dim strC1unit As String
        'Dim strC2unit As String
        'Dim strC1type As String
        'Dim strC2type As String
        Dim lngL_Clabel As Short = 12

        strClabel = Space(KLABEL * lngL_Clabel)
        'Call ZRPD(ifltab(1), cpath, Len(cpath), lngNord, lngNcurve, lngIHORIZ, strC1unit, Len(strC1unit), strC1type, _
        '    Len(strC1type), strC2unit, Len(strC2unit), strC2type, Len(strC2type), VALUES(1), UBound(VALUES) + 1, lngNvals, _
        '    strClabel, lngL_Clabel, KLABEL, lngLabel, lngDummy, 0, lngNheadu, ioStatus)

        SafeNative.ZRPD_(ifltab(0), cpath, Len(cpath), lngNord, lngNcurve, lngIHORIZ, xUnitsBA, xUnitsBA.GetLength(0), xTypeBA, _
           xTypeBA.GetLength(0), yUnitsBA, yUnitsBA.GetLength(0), yTypeBA, yTypeBA.GetLength(0), VALUES(1), UBound(VALUES) + 1, lngNvals, _
           labelBA, lngL_Clabel, KLABEL, lngLabel, lngDummy, 0, lngNheadu, ioStatus)


        ' Copy the units and data type strings
        Units(1) = asciiEncoder.GetString(xUnitsBA).Trim 'RTrim(strC1unit)
        Units(2) = asciiEncoder.GetString(yUnitsBA).Trim 'RTrim(strC2unit)
        Datatype(1) = asciiEncoder.GetString(xTypeBA).Trim 'RTrim(strC1type)
        Datatype(2) = asciiEncoder.GetString(yTypeBA).Trim 'RTrim(strC2type)

        If ioStatus <> 0 OrElse lngNvals = 0 Then
            Exit Sub
        End If

        ' copy the labels
        If lngLabel > 0 Then
            ' labels were returned
            LABEL = True
            ReDim CLABEL(lngNcurve)
            For i = 1 To lngNcurve
                CLABEL(i) = RTrim(Mid(asciiEncoder.GetString(labelBA), 12 * (i - 1) + 1, 12))
            Next i
        Else
            LABEL = False
        End If
    End Sub

    Public Sub hecCloseMessageFile(Optional ByVal blnDeleteMessageAfterClose As Boolean = False)

        Try
            FORTRANCLOSE(CmsgUnit)
            With My.Computer.FileSystem
                If .FileExists(mstrMessageFilename) Then
                    If blnDeleteMessageAfterClose Then .DeleteFile(mstrMessageFilename)
                End If
            End With
        Catch ex As Exception
            'Do nothing.
        End Try
        mstrMessageFilename = ""
    End Sub
    ''' <summary>
    ''' ZINQIR will either return a character string, or an integer number; the other variable will
    ''' be unchanged. Several items refer To the last record read (e.g., record version). If no records
    ''' have been accessed, the variables will be undefined. The information returned Is current To the
    ''' time the file was last accessed by your program. If someone Else Is writing To the file at the same
    ''' time, And you have Not accessed the file for some time, some of the information returned may Not 
    ''' be current(e.g., the number of records in the file)
    ''' </summary>
    ''' <param name="ifltab">The DSS workspace used to manage the DSS file. This is the Output same array used In the ZOPEN Call.</param>
    ''' <param name="strItem">The item to inquire about. Items may be abbreviated to four characters. A list of available items follows.</param>
    ''' <returns>If the item inquired about is returned as a character string, it is returned in this variable.</returns>
    Public Function hecZinqir(ByRef ifltab() As Integer, ByRef strItem As String) As String
        Dim inumb As Integer
        Dim asciiEncoder As System.Text.Encoding = New System.Text.ASCIIEncoding
        Dim cString() As Byte = New Byte((8) - 1) {}


        Dim strout As String

        inumb = -903
        SafeNative.ZINQIR_(ifltab(0), strItem, strItem.Length, cString, cString.GetLength(0), inumb)

        If inumb = -903 Then
            strout = asciiEncoder.GetString(cString).Trim
        Else
            strout = CStr(inumb)
        End If
        Return strout

    End Function

    ''' <summary>
    ''' ZSET provides a means of resetting several default parameters used by DSS. This includes items such As the name Of the program storing data, 
    ''' the DSS file unit number To use, etc. ZSET may be called at any time (before Or after ZOPEN).
    ''' </summary>
    ''' <param name="strItem">The item to be set. Items may be abbreviated to the first four characters of the name.</param>
    ''' <param name="strValue">A character string containing the value to be set. If the parameter to be set is an integer number, this argument Is ignored</param>
    ''' <param name="intValue">The integer number containing the value to be set. If the parameter to be set is a character string, this argument Is ignored.</param>
    Public Sub hecZset(ByVal strItem As String, ByVal strValue As String, ByVal intValue As Integer)
        Dim lValue As Integer
        Dim cItem As String = strItem.ToUpper.PadRight(4)
        Dim cCstr As String = strValue.PadRight(6)
        lValue = intValue
        SafeNative.ZSET_(strItem, cItem.Length, strValue, strValue.Length, intValue)
        'ZSET(cItem, Len(cItem), cCstr, Len(cCstr), lValue)
    End Sub
    Public Sub hecZset(ByVal strItem As String, ByVal strValue As String, ByVal objValue As Object)

        'Dim lngSetting As Long, strSetting As String
        'If IsNumeric(objValue) Then
        '    lngSetting = CLng(objValue)
        '    ZSET_(strItem, Len(strItem), "", 0, lngSetting)
        'Else
        '    strSetting = "" & objValue.ToString
        '    ZSET_(strItem, Len(strItem), strSetting, Len(strSetting), 0)
        'End If
        Dim lValue As Integer
        Dim cItem As String = strItem.ToUpper.PadRight(4)
        Dim cCstr As String = strValue.PadRight(6)
        lValue = CInt(objValue)
        SafeNative.ZSET_(cItem, Len(cItem), cCstr, Len(cCstr), lValue)
    End Sub


    ''UPGRADE_WARNING: Sub Main in a DLL won't get called. Click for more: 'ms-help://MS.VSCC.v80/dv_commoner/local/redirect.htm?keyword="A90BF69E-29C2-4F6F-9E44-92CFC7FAA399"'
    'Public Sub Main()
    '    If LoadLibrary(My.Application.Info.DirectoryPath & "\HecLib.dll") = 0 Then
    '        If LoadLibrary("HecLib.dll") = 0 Then Err.Raise(vbObjectError + Err.LastDllError, My.Application.Info.Title, "Problem loading the HEC library DLL.")
    '    End If
    'End Sub

    Public Sub ConvertHecDateTimeStrings(ByVal StartDateTime As String, ByRef StartDate As String, ByRef StartTime As String)
        If IsDate(StartDateTime) Then
            StartTime = Format(Hour(CDate(StartDateTime)), "00") & Format(Minute(CDate(StartDateTime)), "00")
            StartDate = Month(CDate(StartDateTime)) & "/" & Day(CDate(StartDateTime)) & "/" & Year(CDate(StartDateTime))
        Else
            StartTime = ""
            StartDate = ""
        End If
    End Sub

    Public Sub hecZgpnp(ByVal strCline As String, ByRef strA As String, ByRef strB As String, ByRef strC As String, ByRef strD As String, ByRef strE As String, ByRef strF As String)
        Dim strFixed(6) As String
        Dim lngNparts(6) As Integer
        Dim i As Short
        Dim strItem As String
        Dim strClineFixed As String = strCline '.PadRight(392)

        'ZGPNP(strClineFixed, Len(strClineFixed), strFixed(1), 64, strFixed(2), 64, strFixed(3), 64, strFixed(4), 64, strFixed(5), 64, strFixed(6), 64, lngNparts(1))
        SafeNative.ZGPNP_(strCline, strCline.Length, strFixed(1), 64, strFixed(2), 64, strFixed(3), 64, strFixed(4), 64, strFixed(5), 64, strFixed(6), 64, lngNparts(1))
        If lngNparts(1) = -10 Then
            ' No parts found; skip all
        Else
            For i = 1 To 6
                If lngNparts(i) = -1 Then
                    ' this part not found; skip it
                Else
                    ' this part was found
                    strItem = UCase(Left(FixedLength(strFixed(i), 64), lngNparts(i)))
                    Select Case i
                        Case 1 : strA = strItem
                        Case 2 : strB = strItem
                        Case 3 : strC = strItem
                        Case 4 : strD = strItem
                        Case 5 : strE = strItem
                        Case 6 : strF = strItem
                    End Select
                End If
            Next
        End If
    End Sub
End Module
